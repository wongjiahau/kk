use std::iter::Peekable;
use std::vec::IntoIter;

use crate::ast::*;
use peeking_take_while::PeekableExt;

#[derive(Debug, Clone)]
pub struct Character {
    pub index: usize,
    pub line_number: usize,
    pub column_number: usize,
    pub value: char,
}

pub enum TokenizeError {
    CharacterLiteralCannotBeEmpty {
        position: Position,
    },
    UnterminatedCharacterLiteral {
        position: Position,
    },
    UnterminatedMultilineComment {
        position: Position,
    },
    UnterminatedStringLiteral {
        position: Position,
    },
    InvalidToken {
        error: String,
        position: Position,
    },
    UnknownCharacter {
        character: Character,
    },
    UnexpectedCharacter {
        position: Position,
        expected_character_value: char,
    },
    UnexpectedEOF {
        expected_character_value: char,
    },
}

pub fn tokenize(input: String) -> Result<Vec<Token>, TokenizeError> {
    let characters: Vec<Character> = input
        .split('\n')
        .enumerate()
        .flat_map(|(line_number, line)| {
            let mut characters = line.chars().into_iter().collect::<Vec<char>>();
            characters.push('\n');
            characters
                .into_iter()
                .enumerate()
                .map(move |(column_number, character)| (character, line_number, column_number))
                .collect::<Vec<(char, usize, usize)>>()
        })
        .enumerate()
        .map(
            |(index, (character, line_number, column_number))| Character {
                index,
                value: character,
                line_number,
                column_number,
            },
        )
        .into_iter()
        .collect::<Vec<Character>>();

    let mut it = characters.into_iter().peekable();
    let tokens = eat_token(&mut it, EatMode::Normal)?;
    Ok(tokens)
}

enum EatMode {
    Normal,
    DocumentationCodeSnippet,
}
fn eat_token(
    it: &mut Peekable<IntoIter<Character>>,
    eat_mode: EatMode,
) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = Vec::new();
    while let Some(character) = it.next() {
        match character.value {
            '_' => tokens.push(Token {
                token_type: TokenType::Underscore,
                representation: "_".to_string(),
                position: make_position(character, None),
            }),
            '`' => {
                let backticks = it
                    .by_ref()
                    .peeking_take_while(|character| character.value == '`')
                    .collect::<Vec<Character>>();

                tokens.push(Token {
                    token_type: TokenType::Backtick,
                    representation: "`".to_string(),
                    position: make_position(character, None),
                });
                tokens.extend(
                    backticks
                        .iter()
                        .map(|backtick| Token {
                            token_type: TokenType::Backtick,
                            representation: "`".to_string(),
                            position: make_position(backtick.clone(), None),
                        })
                        .collect::<Vec<Token>>(),
                );
                if let EatMode::DocumentationCodeSnippet = eat_mode {
                    if backticks.len() >= 2 {
                        return Ok(tokens);
                    }
                }
            }
            '#' => {
                let first_hash = character;
                let rest_hashes: Vec<Character> = it
                    .by_ref()
                    .peeking_take_while(|character| character.value == '#')
                    .collect();

                // Then this is a multiline comments (a.k.a documentation string)
                if rest_hashes.len() >= 2 {
                    let characters = {
                        let mut documentation_characters = Vec::new();
                        let mut closing_hash_count = 0;

                        // These are for parsing code snippets
                        let mut opening_backtick_count = 0;

                        loop {
                            if let Some(character) = it.next() {
                                if character.value == '#' {
                                    closing_hash_count += 1;
                                    if closing_hash_count == 3 {
                                        break documentation_characters;
                                    }
                                } else if character.value == '`' {
                                    opening_backtick_count += 1;
                                    if opening_backtick_count == 3 {
                                        let code_snippet_tokens =
                                            eat_token(it, EatMode::DocumentationCodeSnippet)?;

                                        // We need to remove the trailing triple-backticks
                                        let (code_snippet_tokens, triple_backticks) =
                                            code_snippet_tokens
                                                .split_at(code_snippet_tokens.len() - 3);

                                        // Push the code snippet tokens to the current tokens vector
                                        tokens.extend(code_snippet_tokens.to_vec());

                                        // Push the trailing triple-backticks to the documentation
                                        documentation_characters.extend(
                                            triple_backticks.iter().map(|token| Character {
                                                column_number: token.position.column_start,
                                                line_number: token.position.line_start,
                                                index: token.position.character_index_start,
                                                value: '`',
                                            }),
                                        )
                                    }
                                } else {
                                    // reset all counts
                                    closing_hash_count = 0;
                                    opening_backtick_count = 0;
                                }
                                documentation_characters.push(character);
                            } else {
                                return Err(TokenizeError::UnterminatedMultilineComment {
                                    position: make_position(
                                        first_hash,
                                        documentation_characters
                                            .last()
                                            .or_else(|| rest_hashes.last()),
                                    ),
                                });
                            }
                        }
                    };
                    tokens.push(Token {
                        position: make_position(first_hash, characters.last()),
                        token_type: TokenType::MultilineComment,
                        representation: format!(
                            "###{}",
                            characters
                                .iter()
                                .map(|character| character.value.to_string())
                                .collect::<Vec<String>>()
                                .join("")
                        ),
                    })
                }
                // Otherwise this is a single line comment
                else {
                    let content: Vec<Character> = it
                        .by_ref()
                        .peeking_take_while(|character| character.value != '\n')
                        .collect();
                    tokens.push(Token {
                        token_type: TokenType::Comment,
                        representation: format!(
                            "#{}",
                            rest_hashes
                                .iter()
                                .chain(content.iter())
                                .map(|other| other.value.to_string())
                                .collect::<Vec<String>>()
                                .join("")
                        ),
                        position: make_position(first_hash, content.last()),
                    })
                }
            }
            '@' => {
                let first_alias = character.clone();
                let _second_alias = eat_character(it, '@')?;
                let _third_alias = eat_character(it, '@')?;

                let mut characters = Vec::new();
                let mut ending_aliases_count = 0;
                let (characters, last_alias) = loop {
                    if let Some(character) = it.next() {
                        if character.value == '@' {
                            ending_aliases_count += 1;
                            if ending_aliases_count == 3 {
                                break (characters, character);
                            }
                        } else {
                            ending_aliases_count = 0;
                            characters.push(character)
                        }
                    } else {
                        return Err(TokenizeError::UnexpectedEOF {
                            expected_character_value: '@',
                        });
                    }
                };
                tokens.push(Token {
                    token_type: TokenType::JavascriptCode,
                    position: make_position(first_alias, Some(&last_alias)),
                    representation: characters
                        .into_iter()
                        .map(|character| character.value.to_string())
                        .collect::<Vec<String>>()
                        .join(""),
                })
            }
            '\'' => match it.next() {
                Some(quote @ Character { value: '\'', .. }) => {
                    return Err(TokenizeError::CharacterLiteralCannotBeEmpty {
                        position: make_position(character, Some(&quote)),
                    })
                }
                Some(c) => match it.next() {
                    Some(end_quote @ Character { value: '\'', .. }) => tokens.push(Token {
                        token_type: TokenType::Character,
                        representation: format!("'{}'", c.value),
                        position: make_position(character, Some(&end_quote)),
                    }),
                    other => {
                        return Err(TokenizeError::UnterminatedCharacterLiteral {
                            position: make_position(character, other.as_ref()),
                        })
                    }
                },
                None => {
                    return Err(TokenizeError::UnterminatedCharacterLiteral {
                        position: make_position(character, None),
                    })
                }
            },
            '"' => {
                let quote = character.value;
                let content: Vec<Character> = it
                    .by_ref()
                    .peeking_take_while(|character| character.value != quote)
                    .collect();

                match it.by_ref().next() {
                    Some(ending_quote) => tokens.push(Token {
                        token_type: TokenType::String,
                        representation: format!("\"{}\"", stringify(content.clone())),
                        position: make_position(character, Some(&ending_quote)),
                    }),
                    None => {
                        return Err(TokenizeError::UnterminatedStringLiteral {
                            position: make_position(character, content.last()),
                        })
                    }
                }
            }
            '-' | '0'..='9' => {
                if character.value == '-' {
                    if let Some(Character { value: '>', .. }) = it.peek() {
                        let greater_than_character = it.by_ref().next();
                        tokens.push(Token {
                            token_type: TokenType::ThinArrowRight,
                            representation: "->".to_string(),
                            position: make_position(character, greater_than_character.as_ref()),
                        });
                        continue;
                    }
                }

                let intergral = it
                    .by_ref()
                    .peeking_take_while(|character| character.value.is_digit(10))
                    .collect::<Vec<Character>>();

                if character.value == '-' && intergral.is_empty() {
                    tokens.push(Token {
                        token_type: TokenType::Minus,
                        representation: character.value.to_string(),
                        position: Position {
                            line_start: character.line_number,
                            line_end: character.line_number,
                            column_start: character.column_number,
                            column_end: character.column_number,
                            character_index_start: character.index,
                            character_index_end: character.index,
                        },
                    });
                    continue;
                };

                match it.peek() {
                    Some(Character { value: '.', .. }) => {
                        let period = it.next().unwrap();
                        let fractional = it
                            .peeking_take_while(|character| character.value.is_digit(10))
                            .collect::<Vec<Character>>();

                        if fractional.is_empty() {
                            // means there's no fractional part
                            // also, we need to push a period token
                            tokens.push(Token {
                                token_type: TokenType::Integer,
                                representation: format!(
                                    "{}{}",
                                    character.value,
                                    stringify(intergral.clone())
                                ),
                                position: make_position(character, intergral.last()),
                            });
                            tokens.push(Token {
                                token_type: TokenType::Period,
                                representation: period.value.to_string(),
                                position: Position {
                                    line_start: period.line_number,
                                    line_end: period.line_number,
                                    column_start: period.column_number,
                                    column_end: period.column_number,
                                    character_index_start: period.index,
                                    character_index_end: period.index,
                                },
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::Float,
                                representation: format!(
                                    "{}{}.{}",
                                    character.value,
                                    stringify(intergral.clone()),
                                    stringify(fractional.clone())
                                ),
                                position: make_position(character, fractional.last()),
                            })
                        }
                    }
                    _ => {
                        tokens.push(Token {
                            token_type: TokenType::Integer,
                            representation: format!(
                                "{}{}",
                                character.value,
                                stringify(intergral.clone())
                            ),
                            position: make_position(character, intergral.last()),
                        });
                    }
                };
            }
            'A'..='Z' | 'a'..='z' => {
                let characters = it
                    .by_ref()
                    .peeking_take_while(|character| {
                        character.value.is_alphanumeric() || character.value == '_'
                    })
                    .collect::<Vec<Character>>();

                let representation =
                    format!("{}{}", character.value, stringify(characters.clone()));
                tokens.push(Token {
                    token_type: get_token_type(representation.clone()),
                    representation,
                    position: make_position(character, characters.last()),
                })
            }
            '.' => {
                let dots = it
                    .by_ref()
                    .peeking_take_while(|character| character.value == '.')
                    .collect::<Vec<Character>>();

                match dots.len() {
                    0 => tokens.push(Token {
                        token_type: TokenType::Period,
                        representation: ".".to_string(),
                        position: make_position(character, None),
                    }),
                    2 => tokens.push(Token {
                        token_type: TokenType::Spread,
                        representation: "...".to_string(),
                        position: make_position(character, dots.last()),
                    }),
                    _ => {
                        return Err(TokenizeError::InvalidToken {
                            error: "Only one dot (.) or three dots (...) is acceptable".to_string(),
                            position: make_position(character, dots.last()),
                        })
                    }
                }
            }
            '=' => match it.peek() {
                Some(Character { value: '>', .. }) => {
                    let greater_than_character = it.by_ref().next();
                    tokens.push(Token {
                        token_type: TokenType::FatArrowRight,
                        representation: "=>".to_string(),
                        position: make_position(character, greater_than_character.as_ref()),
                    })
                }
                _ => tokens.push(Token {
                    token_type: TokenType::Equals,
                    representation: "=".to_string(),
                    position: make_position(character, None),
                }),
            },
            ':' => match it.peek() {
                Some(Character { value: ':', .. }) => {
                    let colon = it.by_ref().next();
                    tokens.push(Token {
                        token_type: TokenType::DoubleColon,
                        representation: "::".to_string(),
                        position: make_position(character, colon.as_ref()),
                    })
                }
                _ => tokens.push(Token {
                    token_type: TokenType::Colon,
                    representation: ":".to_string(),
                    position: make_position(character, None),
                }),
            },
            other => {
                tokens.push(Token {
                    position: make_position(character.clone(), None),
                    representation: other.to_string(),
                    token_type: match other {
                        '{' => TokenType::LeftCurlyBracket,
                        '}' => TokenType::RightCurlyBracket,
                        '(' => TokenType::LeftParenthesis,
                        ')' => TokenType::RightParenthesis,
                        '[' => TokenType::LeftSquareBracket,
                        ']' => TokenType::RightSquareBracket,
                        ' ' => TokenType::Whitespace,
                        '-' => TokenType::Minus,
                        ',' => TokenType::Comma,
                        '<' => TokenType::LessThan,
                        '>' => TokenType::MoreThan,
                        '|' => TokenType::Pipe,
                        '_' => TokenType::Underscore,
                        '`' => TokenType::Backtick,
                        '\n' => TokenType::Newline,
                        _ => return Err(TokenizeError::UnknownCharacter { character }),
                    },
                });
            }
        }
    }
    Ok(tokens)
}

fn eat_character(
    it: &mut Peekable<IntoIter<Character>>,
    value: char,
) -> Result<Character, TokenizeError> {
    match it.next() {
        Some(character) => {
            if character.value == value {
                Ok(character)
            } else {
                Err(TokenizeError::UnexpectedCharacter {
                    position: make_position(character, None),
                    expected_character_value: value,
                })
            }
        }
        None => Err(TokenizeError::UnexpectedEOF {
            expected_character_value: value,
        }),
    }
}

pub fn make_position(first: Character, last: Option<&Character>) -> Position {
    let last = match last {
        Some(character) => character,
        None => &first,
    };
    Position {
        column_start: first.column_number,
        column_end: last.column_number,
        line_start: first.line_number,
        line_end: last.line_number,
        character_index_start: first.index,
        character_index_end: last.index,
    }
}

pub fn stringify(characters: Vec<Character>) -> String {
    characters
        .into_iter()
        .map(|character| character.value.to_string())
        .collect::<Vec<String>>()
        .join("")
}

pub fn get_token_type(s: String) -> TokenType {
    if s.eq("let") {
        TokenType::KeywordLet
    } else if s.eq("type") {
        TokenType::KeywordType
    } else if s.eq("enum") {
        TokenType::KeywordEnum
    } else if s.eq("do") {
        TokenType::KeywordDo
    } else if s.eq("else") {
        TokenType::KeywordElse
    } else if s.eq("true") {
        TokenType::KeywordTrue
    } else if s.eq("false") {
        TokenType::KeywordFalse
    } else if s.eq("null") {
        TokenType::KeywordNull
    } else if s.eq("import") {
        TokenType::KeywordImport
    } else if s.eq("export") {
        TokenType::KeywordExport
    } else {
        TokenType::Identifier
    }
}
