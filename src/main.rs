pub mod ast;
use ast::*;

// mod unify;
// use unify::*;

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
    // println!("{:#?}", statements);
    Ok(transpile_statements(statements))
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

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
                    token_type: TokenType::Tag,
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
                token_type: TokenType::String,
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
                token_type: TokenType::Identifier,
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
    fn test_number_literal() {
        assert_debug_snapshot!(transpile_source("let x = -123.456".to_string()))
    }

    #[test]
    fn test_array_literal_1() {
        assert_debug_snapshot!(transpile_source("let x = [[],[2],[3,4,],]".to_string()))
    }

    #[test]
    fn test_tag_literal_1() {
        assert_debug_snapshot!(transpile_source("let x = #ok(#some(2))".to_string()))
    }

    #[test]
    fn test_function_1() {
        assert_debug_snapshot!(transpile_source("let x = \\x => 'yo'".to_string()))
    }
    #[test]
    fn test_function_2() {
        assert_debug_snapshot!(transpile_source(
            "let x = \\#red => 'red' \\ #blue => 'blue'".to_string()
        ))
    }

    #[test]
    fn test_function_3() {
        assert_debug_snapshot!(transpile_source(
            "let x = \\#ok(#some(r)) => r \\ _ => 'yo'".to_string()
        ))
    }

    #[test]
    fn test_function_4() {
        assert_debug_snapshot!(transpile_source(
            "let or = \\(#false, #false,) => #false \\(_, _) => #true".to_string()
        ))
    }

    #[test]
    fn test_function_5() {
        assert_debug_snapshot!(transpile_source(
            "let square = \\(x: number): number => x".to_string()
        ))
    }

    #[test]
    fn test_function_call_1() {
        assert_debug_snapshot!(transpile_source("let x = y.square.bomb".to_string()))
    }

    #[test]
    fn test_function_call_2() {
        assert_debug_snapshot!(transpile_source("let x = a.times(b).and(c, d)".to_string()))
    }

    #[test]
    fn test_function_call_3() {
        assert_debug_snapshot!(transpile_source(
            "let x = x.(\\#ok(x) => a \\#no => b)".to_string()
        ))
    }

    #[test]
    fn test_function_call_4() {
        assert_debug_snapshot!(transpile_source(
            "let x = 'hello world'.replace('hello', with: 'bye')".to_string()
        ))
    }

    #[test]
    fn test_record_1() {
        assert_debug_snapshot!(transpile_source(
            "let x = {a: int = {b = c}, d = e}".to_string()
        ))
    }

    #[test]
    fn test_record_2() {
        assert_debug_snapshot!(transpile_source(
            "let x = {...{x = 2, y = 3}, y = 4, z = 5}".to_string()
        ))
    }

    #[test]
    fn test_block_1() {
        assert_debug_snapshot!(transpile_source(
            "let x = let x = a let y = b 'hello world'".to_string()
        ))
    }

    #[test]
    fn test_monadic_binding_1() {
        assert_debug_snapshot!(transpile_source(
            "
            let f = \\(a, b) => 
                let #some(x) = a
                let #ok(y) = b 
                x.plus(y)
            "
            .to_string()
        ))
    }

    #[test]
    fn test_monadic_binding_2() {
        assert_debug_snapshot!(transpile_source(
            "
            let f = \\(a, b) => 
                let #some(x) = a
                let #ok(y) = b 
                    else \\_ => #none
                x.plus(y)
            "
            .to_string()
        ))
    }

    #[test]
    fn test_destructure_record_1() {
        assert_debug_snapshot!(transpile_source(
            "let f = \\{a, b= #ok(#some(c))} => a.plus(c)".to_string()
        ))
    }

    #[test]
    fn test_type_annotation_1() {
        assert_debug_snapshot!(transpile_source("let x: number = 1".to_string()))
    }

    #[test]
    fn test_type_alias_1() {
        assert_debug_snapshot!(transpile_source(
            "type Shape = #circle({radius: string}) | #none | #square(number)".to_string()
        ))
    }
}
