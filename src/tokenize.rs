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

    fn eat_character(&mut self, value: char) -> Result<Character, ParseError> {
        match self.characters_iterator.next() {
            Some(character) => {
                if character.value == value {
                    Ok(character)
                } else {
                    Err(TokenizeError::UnexpectedCharacter {
                        position: make_position(character, None),
                        expected_character_value: value,
                    }
                    .into_parse_error())
                }
            }
            None => Err(TokenizeError::UnexpectedEof {
                expected_character_value: value,
            }
            .into_parse_error()),
        }
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
                '_' => Ok(Some(Token {
                    token_type: TokenType::Underscore,
                    representation: "_".to_string(),
                    position: make_position(character, None),
                })),
                '`' => {
                    let trailing_backticks = self
                        .characters_iterator
                        .by_ref()
                        .peeking_take_while(|character| character.value == '`')
                        .collect::<Vec<Character>>();

                    match trailing_backticks.len() {
                        0 => Ok(Some(Token {
                            token_type: TokenType::Backtick,
                            representation: "`".to_string(),
                            position: make_position(character, None),
                        })),
                        2 => Ok(Some(Token {
                            token_type: TokenType::TripleBacktick,
                            representation: "```".to_string(),
                            position: make_position(character, trailing_backticks.last()),
                        })),
                        _ => panic!("Not implemented yet"),
                    }
                }
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
                            token_type: TokenType::MultilineComment {
                                characters: comment.clone(),
                            },
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
                '@' => {
                    let first_alias = character;
                    let _second_alias = self.eat_character('@')?;
                    let _third_alias = self.eat_character('@')?;

                    let mut characters = Vec::new();
                    let mut ending_aliases_count = 0;
                    let (characters, last_alias) = loop {
                        if let Some(character) = self.characters_iterator.next() {
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
                            return Err(TokenizeError::UnexpectedEof {
                                expected_character_value: '@',
                            }
                            .into_parse_error());
                        }
                    };
                    Ok(Some(Token {
                        token_type: TokenType::JavascriptCode,
                        position: make_position(first_alias, Some(&last_alias)),
                        representation: characters
                            .into_iter()
                            .map(|character| character.value.to_string())
                            .collect::<Vec<String>>()
                            .join(""),
                    }))
                }
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
                '"' => {
                    let start_quote = character;
                    enum StartOf {
                        Nothing,
                        Escape,
                        StringInterpolation,
                    }
                    let mut interpolated_string_sections: Vec<InterpolatedStringSection> = vec![];
                    let (end_quote, characters) = {
                        let mut characters: Vec<Character> = vec![];
                        let mut start_of = StartOf::Nothing;
                        let end_quote = loop {
                            if let Some(character) = self.characters_iterator.next() {
                                let (character, next_start_of) = match &character.value {
                                    '\\' => (
                                        Some(character),
                                        match start_of {
                                            StartOf::Escape => StartOf::Nothing,
                                            _ => StartOf::Escape,
                                        },
                                    ),
                                    '"' => match start_of {
                                        StartOf::Escape => (Some(character), StartOf::Nothing),
                                        _ => break Ok(character),
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
                                                    parser.parse_expression()?,
                                                )),
                                            );
                                            parser.eat_token(TokenType::RightCurlyBracket, None)?;
                                            (None, StartOf::Nothing)
                                        } else {
                                            (Some(character), StartOf::Nothing)
                                        }
                                    }
                                    _ => (Some(character), StartOf::Nothing),
                                };

                                if let Some(character) = character {
                                    characters.push(character.clone());
                                }
                                start_of = next_start_of;
                            } else {
                                break Err(TokenizeError::UnterminatedStringLiteral {
                                    position: make_position(start_quote.clone(), characters.last()),
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
                                token_type: TokenType::InterpolatedString {
                                    start_quote: Box::new(start_quote.clone()),
                                    sections: NonEmpty {
                                        head: head.clone(),
                                        tail: interpolated_string_sections,
                                    },
                                    end_quote: Box::new(end_quote.clone()),
                                },
                                representation: "".to_string(),
                                position: make_position(start_quote, Some(&end_quote)),
                            }))
                        }
                        None => Ok(Some(Token {
                            token_type: TokenType::String,
                            representation: format!("\"{}\"", stringify(characters)),
                            position: make_position(start_quote, Some(&end_quote)),
                        })),
                    }
                }
                '-' | '0'..='9' => {
                    let intergral = self
                        .characters_iterator
                        .by_ref()
                        .peeking_take_while(|character| character.value.is_digit(10))
                        .collect::<Vec<Character>>();

                    if character.value == '-' && intergral.is_empty() {
                        return Ok(Some(Token {
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
                        }));
                    };

                    match self.characters_iterator.peek() {
                        Some(Character { value: '.', .. }) => {
                            let period = self.characters_iterator.next().unwrap();
                            let fractional = self
                                .characters_iterator
                                .peeking_take_while(|character| character.value.is_digit(10))
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
                    let dots = self
                        .characters_iterator
                        .by_ref()
                        .peeking_take_while(|character| character.value == '.')
                        .collect::<Vec<Character>>();

                    match dots.len() {
                        0 => Ok(Some(Token {
                            token_type: TokenType::Period,
                            representation: ".".to_string(),
                            position: make_position(character, None),
                        })),
                        2 => Ok(Some(Token {
                            token_type: TokenType::TriplePeriod,
                            representation: "...".to_string(),
                            position: make_position(character, dots.last()),
                        })),
                        _ => Err(TokenizeError::InvalidToken {
                            error: "Only one dot (.) or three dots(...) are acceptable."
                                .to_string(),
                            position: make_position(character, dots.last()),
                        }
                        .into_parse_error()),
                    }
                }
                '=' => match self.characters_iterator.peek() {
                    Some(Character { value: '>', .. }) => {
                        let greater_than_character = self.characters_iterator.by_ref().next();
                        Ok(Some(Token {
                            token_type: TokenType::FatArrowRight,
                            representation: "=>".to_string(),
                            position: make_position(character, greater_than_character.as_ref()),
                        }))
                    }
                    _ => Ok(Some(Token {
                        token_type: TokenType::Equals,
                        representation: "=".to_string(),
                        position: make_position(character, None),
                    })),
                },
                ':' => match self.characters_iterator.peek() {
                    Some(Character { value: ':', .. }) => {
                        let colon = self.characters_iterator.by_ref().next();
                        Ok(Some(Token {
                            token_type: TokenType::DoubleColon,
                            representation: "::".to_string(),
                            position: make_position(character, colon.as_ref()),
                        }))
                    }
                    _ => Ok(Some(Token {
                        token_type: TokenType::Colon,
                        representation: ":".to_string(),
                        position: make_position(character, None),
                    })),
                },
                other => Ok(Some(Token {
                    position: make_position(character, None),
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
                        '/' => TokenType::Slash,
                        '_' => TokenType::Underscore,
                        '`' => TokenType::Backtick,
                        '!' => TokenType::Bang,
                        '*' => TokenType::Asterisk,
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
    if s.eq("with") {
        TokenType::KeywordWith
    } else if s.eq("switch") {
        TokenType::KeywordSwitch
    } else if s.eq("case") {
        TokenType::KeywordCase
    } else if s.eq("if") {
        TokenType::KeywordIf
    } else if s.eq("else") {
        TokenType::KeywordElse
    } else if s.eq("let") {
        TokenType::KeywordLet
    } else if s.eq("type") {
        TokenType::KeywordType
    } else if s.eq("enum") {
        TokenType::KeywordEnum
    } else if s.eq("do") {
        TokenType::KeywordDo
    } else if s.eq("true") {
        TokenType::KeywordTrue
    } else if s.eq("false") {
        TokenType::KeywordFalse
    } else if s.eq("null") {
        TokenType::KeywordNull
    } else if s.eq("import") {
        TokenType::KeywordImport
    } else if s.eq("from") {
        TokenType::KeywordFrom
    } else if s.eq("as") {
        TokenType::KeywordAs
    } else if s.eq("export") {
        TokenType::KeywordExport
    } else if s.eq("interface") {
        TokenType::KeywordInterface
    } else if s.eq("implements") {
        TokenType::KeywordImplements
    } else if s.eq("where") {
        TokenType::KeywordWhere
    } else {
        TokenType::Identifier
    }
}
