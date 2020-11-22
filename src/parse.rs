use crate::ast::*;

use core::slice::Iter;
use std::iter::Peekable;

pub fn parse_statements(tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokens
        .into_iter()
        .filter(|token| match token.token_type {
            TokenType::Whitespace | TokenType::Newline => false,
            _ => true,
        })
        .collect::<Vec<Token>>();
    let mut it = tokens.iter().peekable();
    parse_statements_(&mut it)
}

pub fn parse_statements_(it: &mut Peekable<Iter<Token>>) -> Result<Vec<Statement>, ParseError> {
    let mut statements = Vec::<Statement>::new();
    loop {
        match it.peek() {
            Some(token) => {
                match token.token_type {
                    TokenType::KeywordLet => {
                        eat_token(it, TokenType::KeywordLet)?;
                        let left = parse_destructure_pattern(it)?;
                        // TODO: eat type annotation
                        eat_token(it, TokenType::Equals)?;
                        let right = parse_expression(it)?;
                        statements.push(Statement::Let {
                            left,
                            right,
                            type_annotation: None,
                        })
                    }
                    TokenType::KeywordType => panic!(),
                    _ => {
                        if !statements.is_empty() {
                            break;
                        } else {
                            let token = it.next().unwrap();
                            return Err(ParseError::InvalidToken {
                                invalid_token: token.clone(),
                                error: "Expected let or type".to_string(),
                                suggestion: None,
                            });
                        }
                    }
                }
            }
            None => {
                if !statements.is_empty() {
                    break;
                } else {
                    return Err(ParseError::UnexpectedEOF {
                        error: "Expected keyword let or type".to_string(),
                        suggestion: None,
                    });
                }
            }
        }
    }
    Ok(statements)
}

pub fn try_eat_token(it: &mut Peekable<Iter<Token>>, token_type: TokenType) -> bool {
    if let Some(token) = it.peek() {
        if token.token_type == token_type {
            let _ = it.next();
            true
        } else {
            false
        }
    } else {
        false
    }
}

pub fn eat_token(it: &mut Peekable<Iter<Token>>, token_type: TokenType) -> Result<(), ParseError> {
    if let Some(token) = it.next() {
        if token.token_type == token_type {
            Ok(())
        } else {
            Err(ParseError::InvalidToken {
                invalid_token: token.clone(),
                error: format!("Expected {:?}", token_type),
                suggestion: None,
            })
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: format!("Expected {:?} but reach EOF", token_type),
            suggestion: Some("Add = after here".to_string()),
        })
    }
}

pub fn parse_function(it: &mut Peekable<Iter<Token>>) -> Result<Function, ParseError> {
    let mut branches = Vec::<FunctionBranch>::new();
    let first_branch = parse_function_branch(it)?;
    while let Some(Token {
        token_type: TokenType::Backslash,
        ..
    }) = it.peek()
    {
        let branch = parse_function_branch(it)?;
        branches.push(branch);
    }
    Ok(Function {
        first_branch,
        branches,
    })
}

pub fn parse_function_branch(it: &mut Peekable<Iter<Token>>) -> Result<FunctionBranch, ParseError> {
    eat_token(it, TokenType::Backslash)?;
    let arguments = parse_function_arguments(it)?;
    eat_token(it, TokenType::ArrowRight)?;
    let body = parse_expression(it)?;
    // TODO: parse return type annotation
    Ok(FunctionBranch {
        arguments,
        body: Box::new(body),
        return_type_annotation: None,
    })
}

pub fn parse_function_arguments(
    it: &mut Peekable<Iter<Token>>,
) -> Result<Vec<FunctionArgument>, ParseError> {
    let mut arguments = Vec::<FunctionArgument>::new();
    let argument = parse_function_argument(it)?;
    arguments.push(argument);
    loop {
        match it.peek() {
            Some(Token {
                token_type: TokenType::ArrowRight,
                ..
            }) => break,
            _ => {
                eat_token(it, TokenType::Comma)?;
                let argument = parse_function_argument(it)?;
                arguments.push(argument);
            }
        }
    }
    Ok(arguments)
}

pub fn parse_function_argument(
    it: &mut Peekable<Iter<Token>>,
) -> Result<FunctionArgument, ParseError> {
    if it.peek().is_some() {
        let destructure_pattern = parse_destructure_pattern(it)?;
        let type_annotation = try_parse_colon_type_annotation(it)?;
        let default_value = try_parse_argument_default_value(it)?;
        Ok(FunctionArgument {
            destructure_pattern,
            type_annotation,
            default_value,
        })
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected function arguments but reach EOF".to_string(),
            suggestion: Some("Add = after here".to_string()),
        })
    }
}

pub fn try_parse_argument_default_value(
    it: &mut Peekable<Iter<Token>>,
) -> Result<Option<Expression>, ParseError> {
    if try_eat_token(it, TokenType::Equals) {
        match parse_expression(it) {
            Ok(expression) => Ok(Some(expression)),
            Err(error) => Err(error),
        }
    } else {
        Ok(None)
    }
}

pub fn try_parse_colon_type_annotation(
    it: &mut Peekable<Iter<Token>>,
) -> Result<Option<TypeAnnotation>, ParseError> {
    if try_eat_token(it, TokenType::Colon) {
        match parse_type_annotation(it) {
            Ok(type_annotation) => Ok(Some(type_annotation)),
            Err(error) => Err(error),
        }
    } else {
        Ok(None)
    }
}

pub fn parse_type_annotation(it: &mut Peekable<Iter<Token>>) -> Result<TypeAnnotation, ParseError> {
    if let Some(token) = it.next() {
        match token.token_type {
            TokenType::Identifier(_) => Ok(TypeAnnotation {
                _type: Type::Name(token.clone()),
                source: None,
            }),
            _ => panic!(),
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected type annotation".to_string(),
            suggestion: None,
        })
    }
}

pub fn parse_function_call(
    it: &mut Peekable<Iter<Token>>,
    first_arg: Expression,
) -> Result<Expression, ParseError> {
    let mut pairs: Vec<(Expression, Vec<FunctionCallArgument>)> = vec![];
    loop {
        if !try_eat_token(it, TokenType::Period) {
            pairs.reverse();
            let (function, arguments) = pairs.pop().unwrap();
            let mut args = vec![FunctionCallArgument {
                argument_name: None,
                value: first_arg,
            }];
            args.extend(arguments);
            let init = Expression {
                value: ExpressionValue::FunctionCall(FunctionCall {
                    function: Box::new(function),
                    arguments: args,
                }),
                inferred_type: None,
            };
            let result = pairs
                .into_iter()
                .fold(init, |first_arg, (function, arguments)| {
                    let mut args = vec![FunctionCallArgument {
                        argument_name: None,
                        value: first_arg,
                    }];
                    args.extend(arguments);
                    Expression {
                        value: ExpressionValue::FunctionCall(FunctionCall {
                            function: Box::new(function),
                            arguments: args,
                        }),
                        inferred_type: None,
                    }
                });
            return Ok(result);
        }
        match it.peek() {
            Some(Token {
                token_type: TokenType::Identifier(_),
                ..
            }) => {
                let token = it.next().unwrap();
                let function_name = Expression {
                    value: ExpressionValue::Variable(token.clone()),
                    inferred_type: None,
                };
                let function_call_arguments = parse_function_call_arguments(it)?;
                pairs.push((function_name, function_call_arguments))
            }
            Some(Token {
                token_type: TokenType::LeftParenthesis,
                ..
            }) => {
                eat_token(it, TokenType::LeftParenthesis)?;
                let function = parse_expression(it)?;
                eat_token(it, TokenType::RightParenthesis)?;
                let function_call_arguments = parse_function_call_arguments(it)?;
                pairs.push((function, function_call_arguments))
            }
            Some(_) => {
                let invalid_token = it.next().unwrap();
                return Err(ParseError::InvalidToken {
                    invalid_token: invalid_token.clone(),
                    error: "Expected variable or left parenthesis".to_string(),
                    suggestion: None,
                });
            }
            None => {
                return Err(ParseError::UnexpectedEOF {
                    error: "Expected variable or left parenthesis".to_string(),
                    suggestion: None,
                })
            }
        }
    }
}

pub fn parse_function_call_arguments(
    it: &mut Peekable<Iter<Token>>,
) -> Result<Vec<FunctionCallArgument>, ParseError> {
    match it.peek() {
        Some(Token {
            token_type: TokenType::LeftParenthesis,
            ..
        }) => {
            eat_token(it, TokenType::LeftParenthesis)?;
            let mut result: Vec<FunctionCallArgument> = vec![];
            loop {
                let argument_name_or_variable = try_parse_identifier(it);
                match argument_name_or_variable {
                    Some(token) => {
                        if try_eat_token(it, TokenType::Colon) {
                            let value = parse_expression(it)?;
                            result.push(FunctionCallArgument {
                                argument_name: Some(token),
                                value,
                            });
                        } else {
                            result.push(FunctionCallArgument {
                                argument_name: None,
                                value: Expression {
                                    value: ExpressionValue::Variable(token),
                                    inferred_type: None,
                                },
                            });
                        }
                    }
                    None => {
                        let value = parse_expression(it)?;
                        result.push(FunctionCallArgument {
                            argument_name: None,
                            value,
                        })
                    }
                }

                if !try_eat_token(it, TokenType::Comma) {
                    break;
                }
            }
            eat_token(it, TokenType::RightParenthesis)?;
            Ok(result)
        }
        _ => Ok(vec![]),
    }
}

pub fn try_parse_identifier(it: &mut Peekable<Iter<Token>>) -> Option<Token> {
    match it.peek() {
        Some(Token {
            token_type: TokenType::Identifier(_),
            ..
        }) => Some(it.next().unwrap().clone()),
        _ => None,
    }
}

pub fn try_parse_function_call(
    it: &mut Peekable<Iter<Token>>,
    first_arg: Expression,
) -> Result<Expression, ParseError> {
    match it.peek() {
        Some(Token {
            token_type: TokenType::Period,
            ..
        }) => {
            let function_call = parse_function_call(it, first_arg)?;
            Ok(function_call)
        }
        _ => Ok(first_arg),
    }
}

pub fn parse_expression(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    if let Some(token) = it.peek() {
        match &token.token_type {
            TokenType::String(_) => {
                let token = it.next().unwrap();
                try_parse_function_call(
                    it,
                    Expression {
                        value: ExpressionValue::String(token.clone()),
                        inferred_type: Some(Type::String),
                    },
                )
            }
            TokenType::Identifier(_) => {
                let token = it.next().unwrap();
                try_parse_function_call(
                    it,
                    Expression {
                        value: ExpressionValue::Variable(token.clone()),
                        inferred_type: None,
                    },
                )
            }
            TokenType::Tag(_) => {
                let token = it.next().unwrap();
                try_parse_function_call(
                    it,
                    Expression {
                        value: ExpressionValue::Tag(token.clone()),
                        inferred_type: Some(Type::Tag(token.clone())),
                    },
                )
            }
            TokenType::Backslash => {
                let function = parse_function(it)?;
                Ok(Expression {
                    value: ExpressionValue::Function(function),
                    inferred_type: None,
                })
            }
            TokenType::LeftCurlyBracket => {
                eat_token(it, TokenType::LeftCurlyBracket)?;
                parse_record_or_block(it)
            }
            TokenType::KeywordLet => parse_let_expression(it),
            TokenType::Number => Ok(Expression {
                value: ExpressionValue::Number(it.next().unwrap().clone()),
                inferred_type: Some(Type::Number),
            }),
            other => {
                panic!("{:#?}", other.clone())
                // match try_parse_function(it) {
                //     Some(function) => Ok(Expression {
                //         value: ExpressionValue::Function(function),
                //         inferred_type: None,
                //     }),
                //     None => {

                //     match other {
                //         TokenType::Identifier(_) => Ok(Expression {
                //             value: ExpressionValue::Variable(*token.clone()),
                //             inferred_type: None
                //         }),
                //         _ => panic!()
                //     }
                //     }
                // }
            }
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected expression".to_string(),
            suggestion: None,
        })
    }
}

pub fn parse_record_or_block(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    match it.peek() {
        Some(Token {
            token_type: TokenType::Identifier(_),
            ..
        }) => parse_record(it),
        Some(_) => parse_let_expression(it),
        None => Err(ParseError::UnexpectedEOF {
            error: "Expected variable or keyword let/type".to_string(),
            suggestion: None,
        }),
    }
}

pub fn parse_record(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    let mut key_value_pairs: Vec<(Token, Expression)> = Vec::new();
    loop {
        match it.peek() {
            Some(Token {
                token_type: TokenType::Identifier(_),
                ..
            }) => {
                let key = it.next().unwrap();
                eat_token(it, TokenType::Colon)?;
                let value = parse_expression(it)?;
                key_value_pairs.push((key.clone(), value))
            }
            Some(&other) => {
                return Err(ParseError::InvalidToken {
                    invalid_token: other.clone(),
                    error: "Expected identifier".to_string(),
                    suggestion: None,
                })
            }
            None => {
                return Err(ParseError::UnexpectedEOF {
                    error: "Expected identifier".to_string(),
                    suggestion: None,
                })
            }
        }
        if !try_eat_token(it, TokenType::Comma) {
            break;
        }
    }
    eat_token(it, TokenType::RightCurlyBracket)?;
    Ok(Expression {
        value: ExpressionValue::Record { key_value_pairs },
        inferred_type: None,
    })
}

pub fn parse_let_expression(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    eat_token(it, TokenType::KeywordLet)?;
    let left = parse_destructure_pattern(it)?;
    eat_token(it, TokenType::Equals)?;
    let right = parse_expression(it)?;
    let return_value = parse_expression(it)?;
    Ok(Expression {
        value: ExpressionValue::Let {
            left,
            right: Box::new(right),
            return_value: Box::new(return_value),
        },
        inferred_type: None,
    })
}

pub fn parse_destructure_pattern(
    it: &mut Peekable<Iter<Token>>,
) -> Result<DestructurePattern, ParseError> {
    if let Some(token) = it.next() {
        match &token.token_type {
            TokenType::Identifier(_) => Ok(DestructurePattern::Identifier(token.clone())),
            TokenType::Tag(_) => match it.peek() {
                Some(Token {
                    token_type: TokenType::LeftParenthesis,
                    ..
                }) => {
                    eat_token(it, TokenType::LeftParenthesis)?;
                    let payload = parse_destructure_pattern(it)?;
                    eat_token(it, TokenType::RightParenthesis)?;
                    Ok(DestructurePattern::Tag {
                        token: token.clone(),
                        payload: Some(Box::new(payload)),
                    })
                }
                _ => Ok(DestructurePattern::Tag {
                    token: token.clone(),
                    payload: None,
                }),
            },
            TokenType::Underscore => Ok(DestructurePattern::Underscore(token.clone())),
            other => panic!("Unimplemented {:#?}", other),
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected destructure pattern".to_string(),
            suggestion: None,
        })
    }
}
