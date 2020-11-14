use core::slice::Iter;
use std::iter::Peekable;

fn main() {
    let tokens = tokenize("let x = '123'".to_string());
    match tokens {
        Ok(tokens) => match parse_statements(tokens) {
            Ok(statements) => println!("{}", transpile_statements(statements)),
            Err(err) => println!("{:#?}", err),
        },
        Err(err) => println!("{:#?}", err),
    }
}

fn transpile_source(source: String) -> Result<String, ParseError> {
    let tokens = tokenize(source)?;
    let statements = parse_statements(tokens)?;
    Ok(transpile_statements(statements))
}

fn transpile_statements(statements: Vec<Statement>) -> String {
    statements.into_iter().map(transpile_statement).collect()
}

fn transpile_statement(statement: Statement) -> String {
    match statement {
        Statement::Let { left, right, .. } => format!(
            "const {} = {}",
            transpile_destructure_pattern(left),
            transpile_expression(right)
        ),
    }
}

fn transpile_destructure_pattern(destructure_pattern: DestructurePattern) -> String {
    match destructure_pattern {
        DestructurePattern::Identifier(i) => i.representation,
    }
}

fn transpile_expression(expression: Expression) -> String {
    match expression {
        Expression::String(s) => s.representation,
    }
}

#[derive(Debug)]
enum Statement {
    Let {
        left: DestructurePattern,
        right: Expression,
        type_annotation: Option<TypeAnnotation>,
    },
}

#[derive(Debug)]
enum TypeAnnotation {}

#[derive(Debug)]
enum DestructurePattern {
    Identifier(Token),
}

#[derive(Debug)]
enum Expression {
    String(Token),
}

#[derive(Debug, PartialEq, Eq)]
enum ParseError {
    InvalidToken {
        invalid_token: Token,
        error: String,
        suggestion: Option<String>,
    },
    UnexpectedEOF {
        error: String,
        suggestion: Option<String>,
    },
}

fn parse_statements(tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
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

fn eat_token(it: &mut Peekable<Iter<Token>>, token_type: TokenType) -> Result<(), ParseError> {
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

fn parse_expression(it: &mut Peekable<Iter<Token>>) -> Result<Expression, ParseError> {
    if let Some(token) = it.next() {
        match &token.token_type {
            TokenType::String(s) => Ok(Expression::String(token.clone())),
            other => panic!("Cannot parse expression {:#?}", other),
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            error: "Expected expression".to_string(),
            suggestion: None,
        })
    }
}

fn parse_destructure_pattern(
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

fn tokenize(input: String) -> Result<Vec<Token>, ParseError> {
    let mut line_number: usize = 0;
    let mut column_number: usize = 0;
    let mut index: usize = 0;
    let mut tokens = Vec::<Token>::new();
    let chars: Vec<char> = input.chars().collect();
    let chars_length = chars.len();
    while index < chars_length {
        let c = chars[index];
        let is_last_char = index == chars_length - 1;
        match c {
            '#' => {
                let column_start = column_number;
                let mut result = "#".to_string();
                loop {
                    if index == chars_length - 1 {
                        tokens.push(Token {
                            token_type: TokenType::Tag(result.to_string()),
                            representation: result.to_string(),
                            position: Position {
                                column_start,
                                column_end: column_number,
                                line_start: line_number,
                                line_end: line_number,
                            },
                        });
                        break;
                    }
                    let next = chars[index + 1];
                    match next {
                        'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
                            result.push(next);
                            index += 1;
                            column_number += 1;
                        }
                        _ => {
                            tokens.push(Token {
                                token_type: TokenType::Tag(result.to_string()),
                                representation: result.to_string(),
                                position: Position {
                                    column_start,
                                    column_end: column_number,
                                    line_start: line_number,
                                    line_end: line_number,
                                },
                            });
                            break;
                        }
                    };
                }
            }
            '\'' | '"' => {
                let column_start = column_number;
                let mut result = c.to_string();
                loop {
                    if index == chars_length - 1 {
                        break;
                    } else {
                        let next = chars[index + 1];
                        index += 1;
                        column_number += 1;
                        if next != c || next == '\n' {
                            result.push(next);
                        } else {
                            result.push(c);
                            break;
                        }
                    }
                }
                tokens.push(Token {
                    token_type: TokenType::String(result.to_string()),
                    representation: result.to_string(),
                    position: Position {
                        column_start,
                        column_end: column_number,
                        line_start: line_number,
                        line_end: line_number,
                    },
                });
            }
            '0'..='9' => {}
            'A'..='Z' | 'a'..='z' | '_' => {
                let column_start = column_number;
                let mut result = c.to_string();
                while index < chars_length - 1 {
                    let next = chars[index + 1];
                    match next {
                        'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
                            result.push(next);
                            index += 1;
                            column_number += 1;
                        }
                        _ => {
                            break;
                        }
                    }
                }
                tokens.push(Token {
                    token_type: get_token_type(result.clone()),
                    representation: result.to_string(),
                    position: Position {
                        column_start,
                        column_end: column_number,
                        line_start: line_number,
                        line_end: line_number,
                    },
                });
            }

            '=' => {
                let eq_token = Token {
                    token_type: TokenType::Equals,
                    representation: "=".to_string(),
                    position: Position {
                        column_start: column_number,
                        column_end: column_number,
                        line_start: line_number,
                        line_end: line_number,
                    },
                };
                if is_last_char {
                    tokens.push(eq_token);
                } else {
                    let next = chars[index + 1];
                    match next {
                        '>' => {
                            tokens.push(Token {
                                token_type: TokenType::ArrowRight,
                                representation: "=>".to_string(),
                                position: Position {
                                    column_start: column_number,
                                    column_end: column_number + 1,
                                    line_start: line_number,
                                    line_end: line_number,
                                },
                            });
                            index += 1;
                            column_number += 1;
                        }
                        _ => tokens.push(eq_token),
                    }
                }
            }
            other => {
                let position = Position {
                    column_start: column_number,
                    column_end: column_number,
                    line_start: line_number,
                    line_end: line_number,
                };
                tokens.push(Token {
                    representation: other.to_string(),
                    token_type: match other {
                        '{' => TokenType::LeftCurlyBracket,
                        '}' => TokenType::RightCurlyBracket,
                        '(' => TokenType::LeftParenthesis,
                        ')' => TokenType::RightParenthesis,
                        '[' => TokenType::LeftSquareBracket,
                        ']' => TokenType::RightSquareBracket,
                        ' ' => TokenType::Whitespace,
                        ':' => TokenType::Colon,
                        ';' => TokenType::Semicolon,
                        '.' => TokenType::Period,
                        ',' => TokenType::Comma,
                        '<' => TokenType::LessThan,
                        '>' => TokenType::MoreThan,
                        '|' => TokenType::Pipe,
                        '\n' => {
                            line_number += 1;
                            column_number = 0;
                            TokenType::Newline
                        }
                        _ => panic!(),
                    },
                    position,
                });
            }
        };
        column_number += 1;
        index += 1;
    }
    Ok(tokens)
}

fn get_token_type(s: String) -> TokenType {
    if s.eq("let") {
        TokenType::KeywordLet
    } else if s.eq("type") {
        TokenType::KeywordType
    } else {
        TokenType::Identifier(s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Token {
    token_type: TokenType,
    position: Position,
    representation: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum TokenType {
    KeywordLet,
    KeywordType,
    Whitespace,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    Newline,
    Colon,
    Semicolon,
    Pipe,
    LessThan,
    MoreThan,
    Equals,
    Period,
    Comma,
    ArrowRight,
    Tag(String),
    Identifier(String),
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Position {
    line_start: usize,
    line_end: usize,
    column_start: usize,
    column_end: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_tokenize_arrow_right() {
        assert_eq!(
            tokenize("=>".to_string()),
            Ok(vec![Token {
                token_type: TokenType::ArrowRight,
                representation: "=>".to_string(),
                position: Position {
                    column_start: 0,
                    column_end: 1,
                    line_start: 0,
                    line_end: 0
                }
            }])
        );
    }

    #[test]
    fn test_tokenize_tag() {
        assert_eq!(
            tokenize("#Hello_world123 ".to_string()),
            Ok(vec![
                Token {
                    token_type: TokenType::Tag("#Hello_world123".to_string()),
                    representation: "#Hello_world123".to_string(),
                    position: Position {
                        column_start: 0,
                        column_end: 14,
                        line_start: 0,
                        line_end: 0
                    }
                },
                Token {
                    token_type: TokenType::Whitespace,
                    representation: " ".to_string(),
                    position: Position {
                        line_start: 0,
                        line_end: 0,
                        column_start: 15,
                        column_end: 15
                    }
                }
            ])
        );
    }

    #[test]
    fn test_tokenize_string() {
        let string = "'qwertyuiop \"world\" 1234567890!'".to_string();
        assert_eq!(
            tokenize(string.clone()),
            Ok(vec![Token {
                token_type: TokenType::String(string.clone()),
                representation: string.clone(),
                position: Position {
                    column_start: 0,
                    column_end: string.len() - 1,
                    line_start: 0,
                    line_end: 0
                }
            }])
        )
    }

    #[test]
    fn test_tokenize_identifier() {
        let string = "Hello_world_123".to_string();
        assert_eq!(
            tokenize(string.clone()),
            Ok(vec![Token {
                token_type: TokenType::Identifier(string.clone()),
                representation: string.clone(),
                position: Position {
                    column_start: 0,
                    column_end: string.len() - 1,
                    line_start: 0,
                    line_end: 0
                }
            }])
        )
    }

    #[test]
    fn test_tokenize_keyword() {
        assert_eq!(
            tokenize("let type".to_string()),
            Ok(vec![
                Token {
                    token_type: TokenType::KeywordLet,
                    representation: "let".to_string(),
                    position: Position {
                        line_start: 0,
                        line_end: 0,
                        column_start: 0,
                        column_end: 2
                    }
                },
                Token {
                    token_type: TokenType::Whitespace,
                    representation: " ".to_string(),
                    position: Position {
                        line_start: 0,
                        line_end: 0,
                        column_start: 3,
                        column_end: 3
                    }
                },
                Token {
                    token_type: TokenType::KeywordType,
                    representation: "type".to_string(),
                    position: Position {
                        line_start: 0,
                        line_end: 0,
                        column_start: 4,
                        column_end: 7
                    }
                }
            ])
        )
    }

    #[test]
    fn test_let_statement_1() {
        assert_eq!(
            transpile_source("let x = '123'".to_string()),
            Ok("const x = '123'".to_string())
        )
    }
}
