fn main() {
    println!("{:#?}", tokenize("Abc = ".to_string()));
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
                        tokens.push(Token {
                            token_type: TokenType::String(result.to_string()),
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
                    index += 1;
                    column_number += 1;
                    if next != c || next == '\n' {
                        result.push(next);
                    } else {
                        result.push(c);
                        tokens.push(Token {
                            token_type: TokenType::String(result.to_string()),
                            position: Position {
                                column_start,
                                column_end: column_number,
                                line_start: line_number,
                                line_end: line_number,
                            },
                        });
                        break;
                    }
                }
            }
            '0'..='9' => {}
            'A'..='Z' | 'a'..='z' | '_' => {
                let column_start = column_number;
                let mut result = c.to_string();
                loop {
                    if index == chars_length - 1 {
                        tokens.push(Token {
                            token_type: TokenType::Identifier(result.to_string()),
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
                                token_type: TokenType::Identifier(result.to_string()),
                                position: Position {
                                    column_start,
                                    column_end: column_number,
                                    line_start: line_number,
                                    line_end: line_number,
                                },
                            });
                            break;
                        }
                    }
                }
            }

            '=' => {
                let eq_token = Token {
                    token_type: TokenType::Equals,
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

#[derive(Debug, PartialEq, Eq)]
struct ParseError {
    column: usize,
    line: usize,
    message: String,
}

#[derive(Debug, PartialEq, Eq)]
struct Token {
    token_type: TokenType,
    position: Position,
}

#[derive(Debug, PartialEq, Eq)]
enum TokenType {
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

#[derive(Debug, PartialEq, Eq)]
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
                    position: Position {
                        column_start: 0,
                        column_end: 14,
                        line_start: 0,
                        line_end: 0
                    }
                },
                Token {
                    token_type: TokenType::Whitespace,
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
                position: Position {
                    column_start: 0,
                    column_end: string.len() - 1,
                    line_start: 0,
                    line_end: 0
                }
            }])
        )
    }
}
