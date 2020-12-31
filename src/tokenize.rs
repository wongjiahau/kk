use crate::ast::*;
use peeking_take_while::PeekableExt;

#[derive(Debug, Clone)]
pub struct Character {
    index: usize,
    line_number: usize,
    column_number: usize,
    value: char,
}

pub fn tokenize(input: String) -> Result<Vec<Token>, ParseError> {
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
        .collect::<Vec<Character>>();

    let mut it = characters.into_iter().peekable();
    let mut tokens = Vec::<Token>::new();

    while let Some(character) = it.next() {
        match character.value {
            '_' => tokens.push(Token {
                token_type: TokenType::Underscore,
                representation: "_".to_string(),
                position: make_position(character, None),
            }),
            '#' => {
                let tagname: Vec<Character> = it
                    .by_ref()
                    .peeking_take_while(|charcater| {
                        charcater.value.is_alphanumeric() || charcater.value == '_'
                    })
                    .collect();

                tokens.push(Token {
                    token_type: TokenType::Tag,
                    representation: "#".to_string() + &stringify(tagname.clone()),
                    position: make_position(character, tagname.last()),
                })
            }
            '\'' | '"' => {
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
                        return Err(ParseError::UnterminatedString {
                            position: make_position(character, content.last()),
                        })
                    }
                }
            }
            '-' | '0'..='9' => {
                let intergral = it
                    .by_ref()
                    .peeking_take_while(|character| character.value.is_digit(10))
                    .collect::<Vec<Character>>();

                let fractional = match it.by_ref().peek() {
                    Some(Character { value: '.', .. }) => {
                        let _ = it.next();
                        let characters = it
                            .by_ref()
                            .peeking_take_while(|character| character.value.is_digit(10))
                            .collect::<Vec<Character>>();
                        Some(characters)
                    }
                    _ => None,
                };
                tokens.push(Token {
                    token_type: TokenType::Number,
                    representation: match &fractional {
                        Some(fractional) => format!(
                            "{}{}.{}",
                            character.value,
                            stringify(intergral.clone()),
                            stringify(fractional.clone())
                        ),
                        None => format!("{}{}", character.value, stringify(intergral.clone())),
                    },
                    position: make_position(
                        character,
                        match &fractional {
                            Some(fractional) => fractional.last(),
                            None => intergral.last(),
                        },
                    ),
                })
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
                        return Err(ParseError::InvalidChar {
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
                        token_type: TokenType::ArrowRight,
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
            other => {
                tokens.push(Token {
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
                        ':' => TokenType::Colon,
                        ';' => TokenType::Semicolon,
                        '+' => TokenType::Plus,
                        '-' => TokenType::Minus,
                        ',' => TokenType::Comma,
                        '<' => TokenType::LessThan,
                        '>' => TokenType::MoreThan,
                        '|' => TokenType::Pipe,
                        '\\' => TokenType::Backslash,
                        '_' => TokenType::Underscore,
                        '\n' => TokenType::Newline,
                        _ => panic!(),
                    },
                });
            }
        }
    }
    Ok(tokens)
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
    } else {
        TokenType::Identifier
    }
}
