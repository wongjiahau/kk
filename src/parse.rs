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
    let mut statements = Vec::<Statement>::new();
    while let Some(token) = it.next() {
        match &token.token_type {
            TokenType::KeywordLet => {
                let left = parse_destructure_pattern(&mut it)?;
                // TODO: eat type annotation
                eat_token(&mut it, TokenType::Equals)?;
                let right = parse_expression(&mut it)?;
                statements.push(Statement::Let {
                    left,
                    right,
                    type_annotation: None,
                })
            }
            TokenType::KeywordType => {}
            _ => {
                return Err(ParseError::InvalidToken {
                    invalid_token: token.clone(),
                    error: "Expected let or type".to_string(),
                    suggestion: None,
                })
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
            error: "Expected = but reach EOF".to_string(),
            suggestion: Some("Add = after here".to_string()),
        })
    }
}

pub fn try_parse_function(original_iterator: &mut Peekable<Iter<Token>>) -> Option<Function> {
    let mut it = original_iterator.clone();
    match parse_function(&mut it) {
        Ok(func) => {
            original_iterator.skip(original_iterator.len() - it.len());
            Some(func)
        }
        Err(_) => None,
    }
}

pub fn parse_function(it: &mut Peekable<Iter<Token>>) -> Result<Function, ParseError> {
    let mut branches = Vec::<FunctionBranch>::new();
    try_eat_token(it, TokenType::Pipe); // Optional preceding pipe operator
    loop {
        let branch = parse_function_branch(it)?;
        println!("parse_function branch {:#?}", branch);
        branches.push(branch);
        if !try_eat_token(it, TokenType::Pipe) {
            println!("parse_function break");
            break;
        }
    }
    Ok(Function { branches })
}

pub fn parse_function_branch(it: &mut Peekable<Iter<Token>>) -> Result<FunctionBranch, ParseError> {
    let arguments = parse_function_arguments(it)?;
    println!("parse_function_branch arguments {:#?}", arguments);
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
    if let Some(token) = it.peek() {
        println!("parse_function_arguments it.next {:#?}", token);
        match token.token_type {
            TokenType::LeftParenthesis => {
                let _ = it.next();
                let mut arguments = Vec::<FunctionArgument>::new();
                loop {
                    let argument = parse_function_argument(it)?;
                    arguments.push(argument);

                    // Allow trailing comma
                    if try_eat_token(it, TokenType::Comma) {
                        if try_eat_token(it, TokenType::RightParenthesis) {
                            break;
                        }
                    } else if try_eat_token(it, TokenType::RightParenthesis) {
                        break;
                    }
                }
                Ok(arguments)
            }
            _ => {
                let destructure_pattern = parse_destructure_pattern(it)?;
                Ok(vec![FunctionArgument {
                    type_annotation: None,
                    default_value: None,
                    destructure_pattern,
                }])
            }
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected function arguments but reach EOF".to_string(),
            suggestion: None,
        })
    }
}

pub fn parse_function_argument(
    it: &mut Peekable<Iter<Token>>,
) -> Result<FunctionArgument, ParseError> {
    if let Some(_) = it.peek() {
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

pub fn parse_expression(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    if let Some(token) = it.peek() {
        match &token.token_type {
            TokenType::String(_) => {
                let token = it.next().unwrap();
                Ok(Expression {
                    value: ExpressionValue::String(token.clone()),
                    inferred_type: Some(Type::String),
                })
            }
            other => {
                let function = parse_function(it)?;
                Ok(Expression {
                    value: ExpressionValue::Function(function),
                    inferred_type: None,
                })
                // if let Some(function) = try_parse_function(it) {
                //     return Ok(Expression {
                //         value: ExpressionValue::Function(function),
                //         inferred_type: None,
                //     });
                // } else {
                //     panic!("{:#?}", other)
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

pub fn parse_destructure_pattern(
    it: &mut Peekable<Iter<Token>>,
) -> Result<DestructurePattern, ParseError> {
    if let Some(token) = it.next() {
        match &token.token_type {
            TokenType::Identifier(_) => Ok(DestructurePattern::Identifier(token.clone())),
            other => panic!("Unimplemented {:#?}", other),
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected destructure pattern".to_string(),
            suggestion: None,
        })
    }
}
