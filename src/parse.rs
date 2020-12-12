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
                        let left = eat_token(it, TokenType::Identifier)?;
                        let type_annotation = try_parse_colon_type_annotation(it)?;
                        eat_token(it, TokenType::Equals)?;
                        let right = parse_expression(it)?;
                        statements.push(Statement::Let {
                            left,
                            right,
                            type_annotation,
                        })
                    }
                    TokenType::KeywordType => {
                        eat_token(it, TokenType::KeywordType)?;
                        let left = parse_identifier(it)?;
                        // TODO: parse type parameter
                        let type_variables = if try_eat_token(it, TokenType::LessThan).is_some() {
                            let mut type_variables = vec![];
                            loop {
                                type_variables.push(parse_identifier(it)?);
                                if try_eat_token(it, TokenType::LessThan).is_none() {
                                    break;
                                }
                            }
                            eat_token(it, TokenType::MoreThan)?;
                            type_variables
                        } else {
                            vec![]
                        };
                        eat_token(it, TokenType::Equals)?;
                        let right = parse_type_annotation(it)?;
                        statements.push(Statement::TypeAlias {
                            left,
                            type_variables,
                            right,
                        })
                    }
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

pub fn try_eat_token(it: &mut Peekable<Iter<Token>>, token_type: TokenType) -> Option<Token> {
    if let Some(token) = it.peek() {
        if token.token_type == token_type {
            match it.next() {
                Some(token) => Some(token.clone()),
                None => None,
            }
        } else {
            None
        }
    } else {
        None
    }
}

pub fn eat_token(
    it: &mut Peekable<Iter<Token>>,
    token_type: TokenType,
) -> Result<Token, ParseError> {
    if let Some(token) = it.next() {
        if token.token_type == token_type {
            Ok(token.clone())
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
            suggestion: None,
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
    let start_token = eat_token(it, TokenType::Backslash)?;
    let arguments = parse_function_arguments(it)?;
    let return_type_annotation = try_parse_colon_type_annotation(it)?;
    eat_token(it, TokenType::ArrowRight)?;
    let body = parse_expression(it)?;
    Ok(FunctionBranch {
        start_token,
        arguments,
        body: Box::new(body),
        return_type_annotation,
    })
}

pub fn parse_function_arguments(
    it: &mut Peekable<Iter<Token>>,
) -> Result<Vec<FunctionArgument>, ParseError> {
    if try_eat_token(it, TokenType::LeftParenthesis).is_some() {
        let mut arguments = Vec::<FunctionArgument>::new();
        loop {
            let argument = parse_function_argument(it)?;
            arguments.push(argument);
            if try_eat_token(it, TokenType::RightParenthesis).is_some() {
                break;
            } else {
                eat_token(it, TokenType::Comma)?;
                if try_eat_token(it, TokenType::RightParenthesis).is_some() {
                    break;
                }
            }
        }
        Ok(arguments)
    } else {
        Ok(vec![parse_function_argument(it)?])
    }
}

pub fn parse_function_argument(
    it: &mut Peekable<Iter<Token>>,
) -> Result<FunctionArgument, ParseError> {
    if it.peek().is_some() {
        let destructure_pattern = parse_destructure_pattern(it)?;
        let type_annotation = try_parse_colon_type_annotation(it)?;
        Ok(FunctionArgument {
            destructure_pattern,
            type_annotation,
        })
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected function arguments but reach EOF".to_string(),
            suggestion: Some("Add = after here".to_string()),
        })
    }
}

pub fn try_parse_colon_type_annotation(
    it: &mut Peekable<Iter<Token>>,
) -> Result<Option<TypeAnnotation>, ParseError> {
    if try_eat_token(it, TokenType::Colon).is_some() {
        match parse_type_annotation(it) {
            Ok(type_annotation) => Ok(Some(type_annotation)),
            Err(error) => Err(error),
        }
    } else {
        Ok(None)
    }
}

pub fn parse_type_annotation(it: &mut Peekable<Iter<Token>>) -> Result<TypeAnnotation, ParseError> {
    let mut type_annotations = Vec::new();
    let first_type_annotation = parse_simple_type_annotation(it)?;
    while try_eat_token(it, TokenType::Pipe).is_some() {
        type_annotations.push(parse_simple_type_annotation(it)?)
    }
    if type_annotations.is_empty() {
        Ok(first_type_annotation)
    } else {
        type_annotations.reverse();
        type_annotations.push(first_type_annotation);
        type_annotations.reverse();
        Ok(TypeAnnotation::Union { type_annotations })
    }
}

pub fn parse_simple_type_annotation(
    it: &mut Peekable<Iter<Token>>,
) -> Result<TypeAnnotation, ParseError> {
    if let Some(token) = it.next() {
        match token.token_type.clone() {
            TokenType::Identifier | TokenType::KeywordNull => {
                let name = token.clone();
                let arguments =
                    if let Some(left_angular_bracket) = try_eat_token(it, TokenType::LessThan) {
                        let mut arguments = vec![];
                        loop {
                            arguments.push(parse_type_annotation(it)?);
                            if try_eat_token(it, TokenType::Comma).is_none() {
                                break;
                            }
                        }
                        let right_angular_bracket = eat_token(it, TokenType::MoreThan)?;
                        Some(NamedTypeAnnotationArguments {
                            left_angular_bracket,
                            arguments,
                            right_angular_bracket,
                        })
                    } else {
                        None
                    };
                Ok(TypeAnnotation::Named { name, arguments })
            }
            TokenType::Tag => {
                let token = token.clone();
                let payload =
                    if let Some(left_parenthesis) = try_eat_token(it, TokenType::LeftParenthesis) {
                        let payload = parse_type_annotation(it)?;
                        let right_parenthesis = eat_token(it, TokenType::RightParenthesis)?;
                        Some(TagTypeAnnotationPayload {
                            left_parenthesis,
                            right_parenthesis,
                            payload: Box::new(payload),
                        })
                    } else {
                        None
                    };
                Ok(TypeAnnotation::Tag { token, payload })
            }
            TokenType::LeftCurlyBracket => {
                let left_curly_bracket = token.clone();
                let mut key_type_annotation_pairs: Vec<(Token, TypeAnnotation)> = Vec::new();
                loop {
                    let key = parse_identifier(it)?;
                    eat_token(it, TokenType::Colon)?;
                    let type_annotation = parse_type_annotation(it)?;
                    key_type_annotation_pairs.push((key, type_annotation));
                    if try_eat_token(it, TokenType::Comma).is_none() {
                        break;
                    }
                }
                let right_curly_bracket = eat_token(it, TokenType::RightCurlyBracket)?;
                Ok(TypeAnnotation::Record {
                    left_curly_bracket,
                    key_type_annotation_pairs,
                    right_curly_bracket,
                })
            }
            TokenType::Backslash => {
                let start_token = token.clone();
                let arguments_types = if try_eat_token(it, TokenType::LeftParenthesis).is_some() {
                    let mut arguments = vec![parse_type_annotation(it)?];
                    if try_eat_token(it, TokenType::Comma).is_some() {
                        arguments.push(parse_type_annotation(it)?)
                    }
                    eat_token(it, TokenType::RightParenthesis)?;
                    arguments
                } else {
                    vec![parse_type_annotation(it)?]
                };

                eat_token(it, TokenType::ArrowRight)?;
                let return_type = parse_type_annotation(it)?;
                Ok(TypeAnnotation::Function {
                    start_token,
                    arguments_types,
                    return_type: Box::new(return_type),
                })
            }
            other => panic!("{:#?}", other),
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
    struct Pair {
        function: Expression,
        arguments: Vec<Expression>,
    }
    let mut pairs: Vec<Pair> = vec![];
    loop {
        if try_eat_token(it, TokenType::Period).is_none() {
            pairs.reverse();
            let Pair {
                function,
                arguments,
            } = pairs.pop().unwrap();
            let mut args = vec![first_arg];
            args.extend(arguments);
            let init = Expression::FunctionCall(FunctionCall {
                function: Box::new(function),
                arguments: args,
            });
            let result = pairs.into_iter().fold(
                init,
                |first_arg,
                 Pair {
                     function,
                     arguments,
                 }| {
                    let mut args = vec![first_arg];
                    args.extend(arguments);
                    Expression::FunctionCall(FunctionCall {
                        function: Box::new(function),
                        arguments: args,
                    })
                },
            );
            return Ok(result);
        }
        match it.peek() {
            Some(Token {
                token_type: TokenType::Identifier,
                ..
            }) => {
                let token = it.next().unwrap();
                let function_name = Expression::Variable(token.clone());
                let function_call_arguments = parse_function_call_arguments(it)?;
                pairs.push(Pair {
                    function: function_name,
                    arguments: function_call_arguments,
                })
            }
            Some(Token {
                token_type: TokenType::LeftParenthesis,
                ..
            }) => {
                eat_token(it, TokenType::LeftParenthesis)?;
                let function = parse_expression(it)?;
                eat_token(it, TokenType::RightParenthesis)?;
                let function_call_arguments = parse_function_call_arguments(it)?;
                pairs.push(Pair {
                    function,
                    arguments: function_call_arguments,
                })
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
) -> Result<Vec<Expression>, ParseError> {
    match it.peek() {
        Some(Token {
            token_type: TokenType::LeftParenthesis,
            ..
        }) => {
            eat_token(it, TokenType::LeftParenthesis)?;
            let mut result: Vec<Expression> = vec![];
            loop {
                result.push(parse_expression(it)?);
                if try_eat_token(it, TokenType::Comma).is_none() {
                    break;
                }
            }
            eat_token(it, TokenType::RightParenthesis)?;
            Ok(result)
        }
        _ => Ok(vec![]),
    }
}

pub fn parse_identifier(it: &mut Peekable<Iter<Token>>) -> Result<Token, ParseError> {
    match it.peek() {
        Some(Token {
            token_type: TokenType::Identifier,
            ..
        }) => Ok(it.next().unwrap().clone()),
        Some(_) => Err(ParseError::InvalidToken {
            invalid_token: it.next().unwrap().clone(),
            error: "Expected identifier".to_string(),
            suggestion: None,
        }),
        _ => Err(ParseError::UnexpectedEOF {
            error: "Expected identifier".to_string(),
            suggestion: None,
        }),
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
            TokenType::Backslash => {
                let function = parse_function(it)?;
                Ok(Expression::Function(Box::new(function)))
            }
            TokenType::KeywordLet => parse_let_expression(it),
            _ => {
                let simple_expression = parse_simple_expression(it)?;
                try_parse_function_call(it, simple_expression)
            }
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected expression".to_string(),
            suggestion: None,
        })
    }
}

pub fn parse_simple_expression(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    if let Some(token) = it.peek() {
        match &token.token_type {
            TokenType::String => {
                let token = it.next().unwrap();
                Ok(Expression::String(token.clone()))
            }
            TokenType::Identifier => {
                let token = it.next().unwrap();
                Ok(Expression::Variable(token.clone()))
            }
            TokenType::Tag => {
                let token = it.next().unwrap();
                let payload =
                    if let Some(left_parenthesis) = try_eat_token(it, TokenType::LeftParenthesis) {
                        let value = parse_expression(it)?;
                        let right_parenthesis = eat_token(it, TokenType::RightParenthesis)?;
                        Some(Box::new(TagPayload {
                            left_parenthesis,
                            right_parenthesis,
                            value,
                        }))
                    } else {
                        None
                    };
                Ok(Expression::Tag {
                    token: token.clone(),
                    payload,
                })
            }
            TokenType::LeftCurlyBracket => parse_record(it),
            TokenType::LeftSquareBracket => parse_array(it),
            TokenType::Number => Ok(Expression::Number(it.next().unwrap().clone())),
            TokenType::KeywordTrue => Ok(Expression::Boolean {
                token: it.next().unwrap().clone(),
                value: true,
            }),
            TokenType::KeywordFalse => Ok(Expression::Boolean {
                token: it.next().unwrap().clone(),
                value: false,
            }),
            TokenType::KeywordNull => Ok(Expression::Null(it.next().unwrap().clone())),
            other => panic!("{:#?}", other.clone()),
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected expression".to_string(),
            suggestion: None,
        })
    }
}

pub fn parse_array(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    let left_square_bracket = eat_token(it, TokenType::LeftSquareBracket)?;
    let mut elements: Vec<Expression> = Vec::new();
    loop {
        if let Some(Token {
            token_type: TokenType::RightSquareBracket,
            ..
        }) = it.peek()
        {
            break;
        };
        elements.push(parse_expression(it)?);
        if try_eat_token(it, TokenType::Comma).is_none() {
            break;
        }
    }
    let right_square_bracket = eat_token(it, TokenType::RightSquareBracket)?;
    Ok(Expression::Array {
        left_square_bracket,
        elements,
        right_square_bracket,
    })
}

pub fn parse_record(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    let left_curly_bracket = eat_token(it, TokenType::LeftCurlyBracket)?;
    let spread = if try_eat_token(it, TokenType::Spread).is_some() {
        let expression = parse_expression(it)?;
        match it.peek() {
            Some(Token {
                token_type: TokenType::RightCurlyBracket,
                ..
            }) => {
                try_eat_token(it, TokenType::Comma);
            }
            _ => {
                if let Err(error) = eat_token(it, TokenType::Comma) {
                    return Err(error);
                }
            }
        };
        Some(Box::new(expression))
    } else {
        None
    };
    let mut key_value_pairs: Vec<RecordKeyValue> = Vec::new();
    loop {
        match it.peek() {
            Some(Token {
                token_type: TokenType::Identifier,
                ..
            }) => {
                let key = it.next().unwrap();
                let type_annotation = try_parse_colon_type_annotation(it)?;
                let value = if try_eat_token(it, TokenType::Equals).is_some() {
                    parse_expression(it)?
                } else {
                    Expression::Variable(key.clone())
                };

                key_value_pairs.push(RecordKeyValue {
                    key: key.clone(),
                    type_annotation,
                    value,
                })
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
        if try_eat_token(it, TokenType::Comma).is_none() {
            break;
        }
    }
    let right_curly_bracket = eat_token(it, TokenType::RightCurlyBracket)?;
    Ok(Expression::Record {
        spread,
        key_value_pairs,
        left_curly_bracket,
        right_curly_bracket,
    })
}

pub fn parse_let_expression(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    let let_keyword = eat_token(it, TokenType::KeywordLet)?;
    let left = parse_destructure_pattern(it)?;
    let type_annotation = if try_eat_token(it, TokenType::Colon).is_some() {
        Some(parse_type_annotation(it)?)
    } else {
        None
    };
    eat_token(it, TokenType::Equals)?;
    let right = parse_expression(it)?;
    let else_return = if try_eat_token(it, TokenType::KeywordElse).is_some() {
        Some(Box::new(parse_expression(it)?))
    } else {
        None
    };
    let return_value = parse_expression(it)?;
    Ok(Expression::Let {
        let_keyword,
        left: Box::new(left),
        type_annotation,
        right: Box::new(right),
        false_branch: else_return,
        true_branch: Box::new(return_value),
    })
}

pub fn parse_destructure_pattern(
    it: &mut Peekable<Iter<Token>>,
) -> Result<DestructurePattern, ParseError> {
    if let Some(token) = it.next() {
        match &token.token_type {
            TokenType::Identifier => Ok(DestructurePattern::Identifier(token.clone())),
            TokenType::Tag => match it.peek() {
                Some(Token {
                    token_type: TokenType::LeftParenthesis,
                    ..
                }) => {
                    let left_parenthesis = eat_token(it, TokenType::LeftParenthesis)?;
                    let destructure_pattern = parse_destructure_pattern(it)?;
                    let right_parenthesis = eat_token(it, TokenType::RightParenthesis)?;
                    Ok(DestructurePattern::Tag {
                        token: token.clone(),
                        payload: Some(Box::new(DestructurePatternTagPayload {
                            left_parenthesis,
                            right_parenthesis,
                            destructure_pattern,
                        })),
                    })
                }
                _ => Ok(DestructurePattern::Tag {
                    token: token.clone(),
                    payload: None,
                }),
            },
            TokenType::Underscore => Ok(DestructurePattern::Underscore(token.clone())),
            TokenType::LeftCurlyBracket => {
                let mut key_value_pairs: Vec<DestructuredRecordKeyValue> = Vec::new();
                loop {
                    if try_eat_token(it, TokenType::RightCurlyBracket).is_some() {
                        break;
                    }
                    let key = parse_identifier(it)?;
                    let type_annotation = try_parse_colon_type_annotation(it)?;
                    let as_value = if try_eat_token(it, TokenType::Equals).is_some() {
                        Some(parse_destructure_pattern(it)?)
                    } else {
                        None
                    };
                    key_value_pairs.push(DestructuredRecordKeyValue {
                        key,
                        type_annotation,
                        as_value,
                        spread: None, // TODO: parse spread
                    });
                    if try_eat_token(it, TokenType::Comma).is_none() {
                        break;
                    }
                }
                let right_curly_bracket = eat_token(it, TokenType::RightCurlyBracket)?;
                Ok(DestructurePattern::Record {
                    left_curly_bracket: token.clone(),
                    key_value_pairs,
                    right_curly_bracket,
                })
            }
            TokenType::Number => Ok(DestructurePattern::Number(token.clone())),
            TokenType::String => Ok(DestructurePattern::String(token.clone())),
            TokenType::KeywordTrue | TokenType::KeywordFalse => {
                Ok(DestructurePattern::Boolean(token.clone()))
            }
            TokenType::KeywordNull => Ok(DestructurePattern::Null(token.clone())),
            TokenType::LeftSquareBracket => {
                let left_square_bracket = token.clone();
                let mut initial_elements: Vec<DestructurePattern> = Vec::new();
                let mut tail_elements: Vec<DestructurePattern> = Vec::new();
                let mut spread: Option<DestructurePatternArraySpread> = None;
                loop {
                    match it.peek() {
                        Some(Token {
                            token_type: TokenType::RightSquareBracket,
                            ..
                        }) => break,
                        Some(Token {
                            token_type: TokenType::Spread,
                            ..
                        }) => {
                            if spread.is_some() {
                                return Err(ParseError::ArrayCannotContainMoreThanOneSpread {
                                    extraneous_spread: it.next().unwrap().clone(),
                                });
                            } else {
                                spread = Some(DestructurePatternArraySpread {
                                    spread_symbol: eat_token(it, TokenType::Spread)?,
                                    binding: try_eat_token(it, TokenType::Identifier),
                                })
                            }
                        }
                        _ => {
                            let destructure_pattern = parse_destructure_pattern(it)?;
                            match spread {
                                Some(_) => tail_elements.push(destructure_pattern),
                                None => initial_elements.push(destructure_pattern),
                            }
                        }
                    }
                    if try_eat_token(it, TokenType::Comma).is_none() {
                        break;
                    }
                }
                let right_square_bracket = eat_token(it, TokenType::RightSquareBracket)?;
                Ok(DestructurePattern::Array {
                    left_square_bracket,
                    right_square_bracket,
                    initial_elements,
                    spread,
                    tail_elements,
                })
            }
            _ => Err(ParseError::ExpectedDestructurePattern {
                token: token.clone(),
            }),
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected destructure pattern".to_string(),
            suggestion: None,
        })
    }
}
