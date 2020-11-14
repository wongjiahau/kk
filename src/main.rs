pub mod ast;
use ast::*;

mod transpile;
use transpile::*;

mod parse;
use parse::*;

mod tokenize;
use tokenize::*;

fn main() {
    println!(
        "{:?}",
        transpile_source("let f = (x: number, y) => 'helo' | x => '2'".to_string())
    )
}

pub fn transpile_source(source: String) -> Result<String, ParseError> {
    let tokens = tokenize(source)?;
    let statements = parse_statements(tokens)?;
    Ok(transpile_statements(statements))
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
                        column_end: 15,
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

    #[test]
    fn test_let_statement_2() {
        assert_eq!(
            transpile_source("let x = y".to_string()),
            Ok("const x = y".to_string())
        )
    }

    #[test]
    fn test_function_1() {
        assert_eq!(
            transpile_source("let x = x => 'yo'".to_string()),
            Ok("const x = (x) => 'yo'".to_string())
        )
    }
    #[test]
    fn test_function_2() {
        assert_eq!(
            transpile_source("let x = #red => 'red' | #blue => 'blue'".to_string()),
            Ok("
const x = (x) => {
  switch(x.$){
    case 'red': return 'red'
    case 'blue': return 'blue'
  }
}"
            .to_string())
        )
    }
}
