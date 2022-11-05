use std::iter::Peekable;
use std::vec::IntoIter;

use crate::non_empty::NonEmpty;
use crate::parse::{ParseErrorKind, Parser};
use crate::{parse::ParseError, raw_ast::*};
use peeking_take_while::PeekableExt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Character {
    pub index: usize,
    pub line_number: usize,
    pub column_number: usize,
    pub value: char,
}

impl Character {
    pub fn position(&self) -> Position {
        Position {
            line_start: self.line_number,
            line_end: self.line_number,
            column_start: self.column_number,
            column_end: self.column_number,
            character_index_start: self.index,
            character_index_end: self.index,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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
    UnexpectedCharacter {
        position: Position,
        expected_character_value: char,
    },
    UnexpectedEof {
        expected_character_value: char,
    },
}

impl TokenizeError {
    pub fn into_parse_error(self) -> ParseError {
        ParseError {
            context: None,
            kind: ParseErrorKind::TokenizeError(self),
        }
    }
}

pub struct Tokenizer {
    characters_iterator: Peekable<IntoIter<Character>>,
    peeked_tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn new(input: String) -> Tokenizer {
        Tokenizer::with_offset(input, 0)
    }

    pub fn with_offset(input: String, offset: usize) -> Tokenizer {
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
                    index: index + offset,
                    value: character,
                    line_number,
                    column_number,
                },
            )
            .into_iter()
            .collect::<Vec<Character>>();

        Tokenizer::from_character_iter(characters.into_iter())
    }

    pub fn from_character_iter(characters: IntoIter<Character>) -> Tokenizer {
        Tokenizer {
            characters_iterator: characters.peekable(),
            peeked_tokens: vec![],
        }
    }

    pub fn remaining_characters(self) -> Vec<Character> {
        self.characters_iterator.collect()
    }

    pub fn peek(&mut self) -> Result<Option<Token>, ParseError> {
        match self.peeked_tokens.last() {
            Some(token) => Ok(Some(token.clone())),
            None => match self.next() {
                Ok(result) => match result {
                    Some(token) => {
                        self.peeked_tokens.push(token.clone());
                        Ok(Some(token))
                    }
                    None => Ok(None),
                },
                Err(error) => Err(error),
            },
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>, ParseError> {
        // Clear peeked token
        if let Some(token) = self.peeked_tokens.pop() {
            return Ok(Some(token));
        }

        if let Some(character) = self.characters_iterator.next() {
            match character.value {
                // Comments
                '/' => match self.characters_iterator.next() {
                    Some(Character { value: '/', .. }) => {
                        let comment = self
                            .characters_iterator
                            .by_ref()
                            .peeking_take_while(|character| character.value != '\n')
                            .collect::<Vec<Character>>();

                        Ok(Some(Token {
                            token_type: TokenType::Comment,
                            position: make_position(character, comment.last()),
                            representation: stringify(comment),
                        }))
                    }
                    Some(Character { value: '*', .. }) => {
                        let mut comment = vec![character.clone()];
                        let mut found_asterisk = false;
                        let comment = loop {
                            match self.characters_iterator.next() {
                                Some(character) => {
                                    if found_asterisk && character.value == '/' {
                                        comment.push(character);
                                        break comment;
                                    }
                                    found_asterisk = character.value == '*';
                                    comment.push(character);
                                }
                                None => {
                                    return Err(ParseError {
                                        context: None,
                                        kind: ParseErrorKind::TokenizeError(
                                            TokenizeError::UnterminatedMultilineComment {
                                                position: make_position(character, comment.last()),
                                            },
                                        ),
                                    })
                                }
                            }
                        };
                        Ok(Some(Token {
                            position: make_position(character, comment.last()),
                            token_type: TokenType::MultilineComment,
                            representation: stringify(comment),
                        }))
                    }
                    Some(other) => Err(ParseError {
                        context: None,
                        kind: ParseErrorKind::TokenizeError(TokenizeError::UnexpectedCharacter {
                            position: make_position(other, None),
                            expected_character_value: '/',
                        }),
                    }),
                    None => Err(ParseError {
                        context: None,
                        kind: ParseErrorKind::TokenizeError(TokenizeError::UnexpectedEof {
                            expected_character_value: '/',
                        }),
                    }),
                },
                // Character
                '\'' => match self.characters_iterator.next() {
                    Some(quote @ Character { value: '\'', .. }) => {
                        Err(TokenizeError::CharacterLiteralCannotBeEmpty {
                            position: make_position(character, Some(&quote)),
                        }
                        .into_parse_error())
                    }
                    Some(c) => match self.characters_iterator.next() {
                        Some(end_quote @ Character { value: '\'', .. }) => Ok(Some(Token {
                            token_type: TokenType::Character,
                            representation: format!("'{}'", c.value),
                            position: make_position(character, Some(&end_quote)),
                        })),
                        other => Err(TokenizeError::UnterminatedCharacterLiteral {
                            position: make_position(character, other.as_ref()),
                        }
                        .into_parse_error()),
                    },
                    None => Err(TokenizeError::UnterminatedCharacterLiteral {
                        position: make_position(character, None),
                    }
                    .into_parse_error()),
                },
                // Quoted Identifers
                // same as MS SQL Identifier using square bracket
                '[' => {
                    let quote = character;
                    let characters = self
                        .characters_iterator
                        .by_ref()
                        .peeking_take_while(|character| character.value != ']')
                        .collect::<Vec<Character>>();

                    match self.characters_iterator.next() {
                        Some(end_quote) => {
                            let representation = format!("{}", stringify(characters.clone()),);
                            Ok(Some(Token {
                                token_type: TokenType::Identifier,
                                representation,
                                position: make_position(quote, Some(&end_quote)),
                            }))
                        }
                        None => panic!("missing closing square bracket `]` for quoted identifier"),
                    }
                }
                // String
                '"' => {
                    let start_quotes = NonEmpty {
                        head: character,
                        tail: self
                            .characters_iterator
                            .by_ref()
                            .peeking_take_while(|character| character.value == '"')
                            .collect::<Vec<Character>>(),
                    };

                    match &start_quotes.tail.split_first() {
                        Some((end_quote, [])) => {
                            // Then this is an empty string
                            return Ok(Some(Token {
                                token_type: TokenType::String(StringLiteral {
                                    start_quotes: start_quotes.clone(),
                                    content: "".to_string(),
                                    end_quotes: NonEmpty {
                                        head: end_quote.clone().clone(),
                                        tail: vec![],
                                    },
                                }),
                                representation: "".to_string(),
                                position: make_position(start_quotes.head, Some(&end_quote)),
                            }));
                        }
                        _ => {}
                    }

                    let allow_escape = start_quotes.len() > 1;

                    enum StartOf {
                        Nothing,
                        Escape,
                        StringInterpolation,
                    }
                    let mut interpolated_string_sections: Vec<InterpolatedStringSection> = vec![];
                    let (end_quotes, characters): (NonEmpty<Character>, Vec<Character>) = {
                        let mut characters: Vec<Character> = vec![];
                        let mut start_of = StartOf::Nothing;
                        let end_quote = loop {
                            if let Some(character) = self.characters_iterator.next() {
                                let (next_characters, next_start_of) = match &character.value {
                                    '\\' => (
                                        Some(vec![character]),
                                        if allow_escape {
                                            match start_of {
                                                StartOf::Escape => StartOf::Nothing,
                                                _ => StartOf::Escape,
                                            }
                                        } else {
                                            StartOf::Nothing
                                        },
                                    ),
                                    '"' => match start_of {
                                        StartOf::Escape => {
                                            (Some(vec![character]), StartOf::Nothing)
                                        }
                                        _ => {
                                            let potential_end_quotes = NonEmpty {
                                                head: character,
                                                tail: self
                                                    .characters_iterator
                                                    .by_ref()
                                                    .peeking_take_while(|character| {
                                                        character.value == '"'
                                                    })
                                                    .collect::<Vec<Character>>(),
                                            };
                                            if potential_end_quotes.len() == start_quotes.len() {
                                                break Ok(potential_end_quotes);
                                            } else {
                                                (
                                                    Some(potential_end_quotes.into_vector()),
                                                    StartOf::Nothing,
                                                )
                                            }
                                        }
                                    },
                                    '$' => (None, StartOf::StringInterpolation),
                                    '{' => {
                                        if let StartOf::StringInterpolation = start_of {
                                            interpolated_string_sections.push(
                                                InterpolatedStringSection::String(stringify(
                                                    characters.clone(),
                                                )),
                                            );
                                            characters.clear();
                                            let mut parser = Parser::new(self);
                                            interpolated_string_sections.push(
                                                InterpolatedStringSection::Expression(Box::new(
                                                    parser.parse_low_precedence_expression()?,
                                                )),
                                            );
                                            parser.eat_token(TokenType::RightCurlyBracket, None)?;
                                            (None, StartOf::Nothing)
                                        } else {
                                            (Some(vec![character]), StartOf::Nothing)
                                        }
                                    }
                                    _ => (Some(vec![character]), StartOf::Nothing),
                                };

                                if let Some(next_characters) = next_characters {
                                    characters.extend(next_characters.clone());
                                }
                                start_of = next_start_of;
                            } else {
                                break Err(TokenizeError::UnterminatedStringLiteral {
                                    position: make_position(
                                        start_quotes.head.clone(),
                                        characters.last(),
                                    ),
                                }
                                .into_parse_error());
                            }
                        }?;
                        (end_quote, characters)
                    };

                    match interpolated_string_sections.split_first() {
                        Some((head, interpolated_string_sections)) => {
                            let interpolated_string_sections = {
                                let mut interpolated_string_sections =
                                    interpolated_string_sections.to_vec();
                                interpolated_string_sections
                                    .push(InterpolatedStringSection::String(stringify(characters)));
                                interpolated_string_sections
                            };
                            Ok(Some(Token {
                                token_type: TokenType::InterpolatedString(InterpolatedString {
                                    start_quotes: start_quotes.clone(),
                                    sections: NonEmpty {
                                        head: head.clone(),
                                        tail: interpolated_string_sections,
                                    },
                                    end_quotes: end_quotes.clone(),
                                }),
                                representation: "".to_string(),
                                position: make_position(
                                    start_quotes.head,
                                    Some(&end_quotes.last()),
                                ),
                            }))
                        }

                        // If there's no interpolated sections, then it's a simple string
                        None => Ok(Some(Token {
                            token_type: TokenType::String(StringLiteral {
                                start_quotes: start_quotes.clone(),
                                content: stringify(characters.clone()),
                                end_quotes: end_quotes.clone(),
                            }),
                            representation: stringify(characters),
                            position: make_position(
                                start_quotes.head.clone(),
                                Some(&end_quotes.last()),
                            ),
                        })),
                    }
                }
                // Number
                '-' | '0'..='9' => {
                    if let Some(next_character) =
                        self.characters_iterator.next_if(|c| c.value == '>')
                    {
                        Ok(Some(Token {
                            token_type: TokenType::ArrowRight,
                            representation: "->".to_string(),
                            position: make_position(character, Some(&next_character)),
                        }))
                    } else {
                        let intergral = self
                            .characters_iterator
                            .by_ref()
                            .peeking_take_while(|character| character.value.is_digit(10))
                            .collect::<Vec<Character>>();

                        if character.value == '-' && intergral.is_empty() {
                            let characters = self
                                .characters_iterator
                                .by_ref()
                                .peeking_take_while(|character| is_symbol(character.value))
                                .collect::<Vec<Character>>();

                            let representation =
                                format!("{}{}", character.value, stringify(characters.clone()));
                            Ok(Some(Token {
                                token_type: TokenType::Identifier,
                                representation,
                                position: make_position(character, characters.last()),
                            }))
                        } else {
                            match self.characters_iterator.peek() {
                                Some(Character { value: '.', .. }) => {
                                    let period = self.characters_iterator.next().unwrap();
                                    let fractional = self
                                        .characters_iterator
                                        .peeking_take_while(|character| {
                                            character.value.is_digit(10)
                                        })
                                        .collect::<Vec<Character>>();

                                    if fractional.is_empty() {
                                        // means there's no fractional part
                                        // that means the next token is a Period
                                        self.peeked_tokens.push(Token {
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
                                        Ok(Some(Token {
                                            token_type: TokenType::Integer,
                                            representation: format!(
                                                "{}{}",
                                                character.value,
                                                stringify(intergral.clone())
                                            ),
                                            position: make_position(character, intergral.last()),
                                        }))
                                    } else {
                                        Ok(Some(Token {
                                            token_type: TokenType::Float,
                                            representation: format!(
                                                "{}{}.{}",
                                                character.value,
                                                stringify(intergral),
                                                stringify(fractional.clone())
                                            ),
                                            position: make_position(character, fractional.last()),
                                        }))
                                    }
                                }
                                _ => Ok(Some(Token {
                                    token_type: TokenType::Integer,
                                    representation: format!(
                                        "{}{}",
                                        character.value,
                                        stringify(intergral.clone())
                                    ),
                                    position: make_position(character, intergral.last()),
                                })),
                            }
                        }
                    }
                }
                // Tag or HashLeftCurlyBracket
                '#' => match self.characters_iterator.by_ref().peek() {
                    Some(Character { value: '{', .. }) => Ok(Some(Token {
                        token_type: TokenType::HashLeftCurlyBracket,
                        representation: "#{".to_string(),
                        position: make_position(
                            character,
                            self.characters_iterator.by_ref().next().as_ref(),
                        ),
                    })),
                    _ => {
                        let characters = self
                            .characters_iterator
                            .by_ref()
                            .peeking_take_while(|character| {
                                character.value.is_alphanumeric() || character.value == '_'
                            })
                            .collect::<Vec<Character>>();

                        let representation =
                            format!("{}{}", character.value, stringify(characters.clone()));
                        Ok(Some(Token {
                            token_type: TokenType::Tag,
                            representation,
                            position: make_position(character, characters.last()),
                        }))
                    }
                },
                // Identifier
                'A'..='Z' | 'a'..='z' => {
                    let characters = self
                        .characters_iterator
                        .by_ref()
                        .peeking_take_while(|character| {
                            character.value.is_alphanumeric() || character.value == '_'
                        })
                        .collect::<Vec<Character>>();

                    let representation =
                        format!("{}{}", character.value, stringify(characters.clone()));
                    Ok(Some(Token {
                        token_type: get_token_type(representation.clone()),
                        representation,
                        position: make_position(character, characters.last()),
                    }))
                }
                '.' => {
                    let characters = self
                        .characters_iterator
                        .by_ref()
                        .peeking_take_while(|character| character.value == '.')
                        .collect::<Vec<Character>>();
                    match characters.len() {
                        0 => Ok(Some(Token {
                            token_type: TokenType::Period,
                            representation: ".".to_string(),
                            position: make_position(character, None),
                        })),
                        1 => Ok(Some(Token {
                            token_type: TokenType::DoublePeriod,
                            representation: "..".to_string(),
                            position: make_position(character, characters.last()),
                        })),
                        _ => panic!(),
                    }
                }
                ':' => Ok(Some(Token {
                    token_type: TokenType::Colon,
                    representation: ":".to_string(),
                    position: make_position(character, None),
                })),
                '=' => Ok(Some(Token {
                    token_type: TokenType::Equals,
                    representation: "=".to_string(),
                    position: make_position(character, None),
                })),
                '|' => Ok(Some(Token {
                    token_type: TokenType::Pipe,
                    representation: "|".to_string(),
                    position: make_position(character, None),
                })),
                '<' => Ok(Some(Token {
                    token_type: TokenType::LessThan,
                    representation: "<".to_string(),
                    position: make_position(character, None),
                })),
                '>' => Ok(Some(Token {
                    token_type: TokenType::MoreThan,
                    representation: ">".to_string(),
                    position: make_position(character, None),
                })),
                '_' => Ok(Some(Token {
                    token_type: TokenType::Underscore,
                    representation: "_".to_string(),
                    position: make_position(character, None),
                })),
                '!' => Ok(Some(Token {
                    token_type: TokenType::Bang,
                    representation: "!".to_string(),
                    position: make_position(character, None),
                })),
                '~' => Ok(Some(Token {
                    token_type: TokenType::Tilde,
                    representation: "~".to_string(),
                    position: make_position(character, None),
                })),
                '\\' => Ok(Some(Token {
                    token_type: TokenType::Backslash,
                    representation: "\\".to_string(),
                    position: make_position(character, None),
                })),
                // Symbolic identifer
                other if is_symbol(other) => {
                    let characters = self
                        .characters_iterator
                        .by_ref()
                        .peeking_take_while(|character| is_symbol(character.value))
                        .collect::<Vec<Character>>();

                    let representation =
                        format!("{}{}", character.value, stringify(characters.clone()));
                    Ok(Some(Token {
                        token_type: TokenType::Operator,
                        representation,
                        position: make_position(character, characters.last()),
                    }))
                }
                other => Ok(Some(Token {
                    position: make_position(character, None),
                    representation: other.to_string(),
                    token_type: match other {
                        '.' => TokenType::Period,
                        '{' => TokenType::LeftCurlyBracket,
                        '}' => TokenType::RightCurlyBracket,
                        '(' => TokenType::LeftParenthesis,
                        ')' => TokenType::RightParenthesis,
                        '[' => TokenType::LeftSquareBracket,
                        ']' => TokenType::RightSquareBracket,
                        '<' => TokenType::LessThan,
                        '>' => TokenType::MoreThan,
                        ' ' => TokenType::Whitespace,
                        ',' => TokenType::Comma,
                        ';' => TokenType::Semicolon,
                        '\n' => TokenType::Newline,
                        other => TokenType::Other(other),
                    },
                })),
            }
        } else {
            Ok(None)
        }
    }
}

fn is_symbol(c: char) -> bool {
    "!@#$%^&*_-<>=+/|?~".contains(c)
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
    match s.as_str() {
        "entry" => TokenType::KeywordEntry,
        "let" => TokenType::KeywordLet,
        "type" => TokenType::KeywordType,
        "import" => TokenType::KeywordImport,
        "public" => TokenType::KeywordPublic,
        "export" => TokenType::KeywordExport,
        "exists" => TokenType::KeywordExists,
        "as" => TokenType::KeywordAs,
        "case" => TokenType::KeywordCase,
        _ => TokenType::Identifier,
    }
}
