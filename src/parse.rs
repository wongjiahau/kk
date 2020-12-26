use crate::ast::*;
use core::slice::Iter;
use std::iter::Peekable;

pub fn parse_statements(tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokens
        .into_iter()
        .filter(|token| !matches!(token.token_type, TokenType::Whitespace | TokenType::Newline))
        .collect::<Vec<Token>>();
    let mut it = tokens.iter().peekable();
    parse_statements_(&mut it)
}

pub fn parse_statements_(it: &mut Peekable<Iter<Token>>) -> Result<Vec<Statement>, ParseError> {
    let mut statements = Vec::<Statement>::new();
    loop {
        match it.peek() {
            Some(token) => match token.token_type {
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
                    let type_variables = try_parse_type_variables(it)?;
                    eat_token(it, TokenType::Equals)?;
                    let right = parse_type_annotation(it)?;
                    statements.push(Statement::Type {
                        left,
                        type_variables,
                        right,
                    })
                }
                TokenType::KeywordEnum => {
                    eat_token(it, TokenType::KeywordEnum)?;
                    let name = parse_identifier(it)?;
                    let type_variables = try_parse_type_variables(it)?;
                    eat_token(it, TokenType::Equals)?;

                    let mut tags = vec![parse_enum_tag(it)?];

                    while let Some(Token {
                        token_type: TokenType::Tag,
                        ..
                    }) = it.peek()
                    {
                        tags.push(parse_enum_tag(it)?)
                    }
                    statements.push(Statement::Enum {
                        name,
                        type_variables,
                        tags,
                    })
                }
                _ => {
                    if !statements.is_empty() {
                        break;
                    } else {
                        let token = it.next().unwrap();
                        return Err(ParseError::InvalidToken {
                            invalid_token: token.clone(),
                            error: "Expected keyword `let` or `type` or `enum`".to_string(),
                            suggestion: None,
                        });
                    }
                }
            },
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

pub fn parse_enum_tag(it: &mut Peekable<Iter<Token>>) -> Result<EnumTag, ParseError> {
    let tagname = eat_token(it, TokenType::Tag)?;
    let payload = if let Some(left_parenthesis) = try_eat_token(it, TokenType::LeftParenthesis) {
        let type_annotation = parse_type_annotation(it)?;
        let right_parenthesis = eat_token(it, TokenType::RightParenthesis)?;
        Some(EnumTagPayload {
            left_parenthesis,
            right_parenthesis,
            type_annotation: Box::new(type_annotation),
        })
    } else {
        None
    };

    Ok(EnumTag { tagname, payload })
}

pub fn try_parse_type_variables(it: &mut Peekable<Iter<Token>>) -> Result<Vec<Token>, ParseError> {
    if try_eat_token(it, TokenType::LessThan).is_some() {
        let mut type_variables = vec![];
        loop {
            type_variables.push(parse_identifier(it)?);
            if try_eat_token(it, TokenType::LessThan).is_none() {
                break;
            }
        }
        eat_token(it, TokenType::MoreThan)?;
        Ok(type_variables)
    } else {
        Ok(Vec::new())
    }
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
    let ParseFunctionArgumentResult {
        first_argument,
        rest_arguments,
    } = parse_function_arguments(it)?;
    let return_type_annotation = try_parse_colon_type_annotation(it)?;
    eat_token(it, TokenType::ArrowRight)?;
    let body = parse_expression(it)?;
    Ok(FunctionBranch {
        start_token,
        first_argument,
        rest_arguments,
        body: Box::new(body),
        return_type_annotation,
    })
}

pub struct ParseFunctionArgumentResult {
    pub first_argument: Box<FunctionArgument>,
    pub rest_arguments: Option<FunctionBranchRestArguments>,
}
pub fn parse_function_arguments(
    it: &mut Peekable<Iter<Token>>,
) -> Result<ParseFunctionArgumentResult, ParseError> {
    if let Some(left_parenthesis) = try_eat_token(it, TokenType::LeftParenthesis) {
        let first_argument = Box::new(parse_function_argument(it)?);
        let mut rest_arguments = Vec::<FunctionArgument>::new();
        let right_parenthesis = loop {
            if try_eat_token(it, TokenType::Comma).is_none() {
                break eat_token(it, TokenType::RightParenthesis)?;
            }
            let argument = parse_function_argument(it)?;
            rest_arguments.push(argument);
            if let Some(right_parenthesis) = try_eat_token(it, TokenType::RightParenthesis) {
                break right_parenthesis;
            } else {
                eat_token(it, TokenType::Comma)?;
                if let Some(right_parenthesis) = try_eat_token(it, TokenType::RightParenthesis) {
                    break right_parenthesis;
                }
            }
        };
        Ok(ParseFunctionArgumentResult {
            first_argument,
            rest_arguments: Some(FunctionBranchRestArguments {
                left_parenthesis,
                rest_arguments,
                right_parenthesis,
            }),
        })
    } else {
        Ok(ParseFunctionArgumentResult {
            first_argument: Box::new(parse_function_argument(it)?),
            rest_arguments: None,
        })
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
                let (first_argument_type, rest_arguments_types) =
                    if try_eat_token(it, TokenType::LeftParenthesis).is_some() {
                        let first_argument = parse_type_annotation(it)?;
                        let mut arguments_types = vec![];
                        if try_eat_token(it, TokenType::Comma).is_some() {
                            arguments_types.push(parse_type_annotation(it)?)
                        }
                        eat_token(it, TokenType::RightParenthesis)?;
                        (first_argument, arguments_types)
                    } else {
                        (parse_type_annotation(it)?, vec![])
                    };

                eat_token(it, TokenType::ArrowRight)?;
                let return_type = parse_type_annotation(it)?;
                Ok(TypeAnnotation::Function {
                    start_token,
                    first_argument_type: Box::new(first_argument_type),
                    rest_arguments_types,
                    return_type: Box::new(return_type),
                })
            }
            _ => Err(ParseError::ExpectedTypeAnnotation {
                actual_token: token.clone(),
            }),
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
    first_argument: Expression,
) -> Result<Expression, ParseError> {
    if try_eat_token(it, TokenType::Period).is_none() {
        return Ok(first_argument);
    }
    let first_argument = match it.peek() {
        Some(Token {
            token_type: TokenType::Identifier,
            ..
        }) => {
            let token = it.next().unwrap();
            let function_name = Box::new(Expression::Variable(token.clone()));
            let function_call_arguments = parse_function_call_rest_arguments(it)?;
            Ok(Expression::FunctionCall(FunctionCall {
                first_argument: Box::new(first_argument),
                rest_arguments: function_call_arguments,
                function: function_name,
            }))
        }
        Some(Token {
            token_type: TokenType::LeftParenthesis,
            ..
        }) => {
            eat_token(it, TokenType::LeftParenthesis)?;
            let function = Box::new(parse_expression(it)?);
            eat_token(it, TokenType::RightParenthesis)?;
            let function_call_arguments = parse_function_call_rest_arguments(it)?;
            Ok(Expression::FunctionCall(FunctionCall {
                first_argument: Box::new(first_argument),
                rest_arguments: function_call_arguments,
                function,
            }))
        }
        Some(_) => {
            let invalid_token = it.next().unwrap();
            Err(ParseError::InvalidToken {
                invalid_token: invalid_token.clone(),
                error: "Expected variable or left parenthesis".to_string(),
                suggestion: None,
            })
        }
        None => Err(ParseError::UnexpectedEOF {
            error: "Expected variable or left parenthesis".to_string(),
            suggestion: None,
        }),
    }?;
    parse_function_call(it, first_argument)
}

pub fn parse_function_call_rest_arguments(
    it: &mut Peekable<Iter<Token>>,
) -> Result<Option<FunctionCallRestArguments>, ParseError> {
    match it.peek() {
        Some(Token {
            token_type: TokenType::LeftParenthesis,
            ..
        }) => {
            let left_parenthesis = eat_token(it, TokenType::LeftParenthesis)?;
            let mut arguments: Vec<Expression> = vec![];
            loop {
                arguments.push(parse_expression(it)?);
                if try_eat_token(it, TokenType::Comma).is_none() {
                    break;
                }
            }
            let right_parenthesis = eat_token(it, TokenType::RightParenthesis)?;
            Ok(Some(FunctionCallRestArguments {
                left_parenthesis,
                arguments,
                right_parenthesis,
            }))
        }
        _ => Ok(None),
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
                        tagname: token.clone(),
                        payload: Some(Box::new(DestructurePatternTagPayload {
                            left_parenthesis,
                            right_parenthesis,
                            destructure_pattern,
                        })),
                    })
                }
                _ => Ok(DestructurePattern::Tag {
                    tagname: token.clone(),
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
            TokenType::KeywordTrue => Ok(DestructurePattern::Boolean {
                token: token.clone(),
                value: true,
            }),
            TokenType::KeywordFalse => Ok(DestructurePattern::Boolean {
                token: token.clone(),
                value: false,
            }),
            TokenType::KeywordNull => Ok(DestructurePattern::Null(token.clone())),
            TokenType::LeftSquareBracket => {
                let left_square_bracket = token.clone();
                if let Some(right_square_bracket) = try_eat_token(it, TokenType::RightSquareBracket)
                {
                    Ok(DestructurePattern::Array {
                        left_square_bracket,
                        right_square_bracket,
                        spread: None,
                    })
                } else {
                    let left = Box::new(parse_destructure_pattern(it)?);
                    eat_token(it, TokenType::Comma)?;
                    let spread_token = eat_token(it, TokenType::Spread)?;
                    let right = Box::new(parse_destructure_pattern(it)?);
                    let right_square_bracket = eat_token(it, TokenType::RightSquareBracket)?;
                    Ok(DestructurePattern::Array {
                        left_square_bracket,
                        right_square_bracket,
                        spread: Some(DestructurePatternArraySpread {
                            left,
                            spread_token,
                            right,
                        }),
                    })
                }
                // let mut initial_elements: Vec<DestructurePattern> = Vec::new();
                // let mut tail_elements: Vec<DestructurePattern> = Vec::new();
                // let mut spread: Option<DestructurePatternArraySpread> = None;
                // loop {
                //     match it.peek() {
                //         Some(Token {
                //             token_type: TokenType::RightSquareBracket,
                //             ..
                //         }) => break,
                // Some(Token {
                //     token_type: TokenType::Spread,
                //     ..
                // }) => {
                //     if spread.is_some() {
                //         return Err(ParseError::ArrayCannotContainMoreThanOneSpread {
                //             extraneous_spread: it.next().unwrap().clone(),
                //         });
                //     } else {
                //         spread = Some(DestructurePatternArraySpread {
                //             spread_symbol: eat_token(it, TokenType::Spread)?,
                //             binding: try_eat_token(it, TokenType::Identifier),
                //         })
                //     }
                // }
                // _ => {

                //     let destructure_pattern = parse_destructure_pattern(it)?;
                //     match spread {
                //         Some(_) => tail_elements.push(destructure_pattern),
                //         None => initial_elements.push(destructure_pattern),
                //     }
                // }
                // }
                // if try_eat_token(it, TokenType::Comma).is_none() {
                // break;
                // }
                // }
                // let right_square_bracket = eat_token(it, TokenType::RightSquareBracket)?;
                // Ok(DestructurePattern::Array {
                //     left_square_bracket,
                //     right_square_bracket,
                //     initial_elements,
                //     spread,
                //     tail_elements,
                // })
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
