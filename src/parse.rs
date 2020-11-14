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

pub fn parse_expression(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    if let Some(token) = it.next() {
        match &token.token_type {
            TokenType::String(_) => Ok(Expression::String(token.clone())),
            other => panic!("Cannot parse expression {:#?}", other),
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
