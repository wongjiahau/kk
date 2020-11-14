use crate::ast::*;

pub fn tokenize(input: String) -> Result<Vec<Token>, ParseError> {
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

pub fn get_token_type(s: String) -> TokenType {
    if s.eq("let") {
        TokenType::KeywordLet
    } else if s.eq("type") {
        TokenType::KeywordType
    } else {
        TokenType::Identifier(s)
    }
}
