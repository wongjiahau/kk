pub mod ast;
use ast::*;

mod environment;

mod unify;
use unify::*;

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

#[derive(Debug)]
pub enum CompileError {
    UnifyError(UnifyError),
    ParseError(ParseError),
}

pub fn type_check_source(source: String) -> Result<(), CompileError> {
    match source_to_statements(source) {
        Err(parse_error) => Err(CompileError::ParseError(parse_error)),
        Ok(statements) => match unify_program(Program {
            source: Source::NonFile {
                env_name: "TESTING".to_string(),
            },
            statements,
        }) {
            Err(unify_error) => Err(CompileError::UnifyError(unify_error)),
            Ok(_) => Ok(()),
        },
    }
}

pub fn source_to_statements(source: String) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokenize(source)?;
    parse_statements(tokens)
}

pub fn transpile_source(source: String) -> Result<String, ParseError> {
    let statements = source_to_statements(source)?;
    // println!("{:#?}", statements);
    Ok(transpile_statements(statements))
}

#[cfg(test)]
mod test_tokenize {
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
}

#[cfg(test)]
mod test_transpile {
    use super::*;
    use insta::assert_debug_snapshot;

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

    #[test]
    fn transpile_let_destructure_number_1() {
        assert_debug_snapshot!(transpile_source("let f = \\x => let 2 = x 10".to_string()))
    }

    #[test]
    fn transpile_let_destructure_string_1() {
        assert_debug_snapshot!(transpile_source(
            "let f = \\x => let 'yo' = x 'hey'".to_string()
        ))
    }
}

#[cfg(test)]
mod test_type_check {
    use super::*;
    use insta::assert_debug_snapshot;
    #[test]
    fn test_type_check_simple_1() {
        assert_debug_snapshot!(type_check_source("let x: string = 123".to_string()))
    }

    #[test]
    fn test_type_check_simple_2() {
        assert_debug_snapshot!(type_check_source("let x: number = 123".to_string()))
    }

    #[test]
    fn test_type_check_function_1() {
        assert_debug_snapshot!(type_check_source(
            "
            let f = 
                \\(x: number) => 1
                \\(x, y) => 1
            "
            .to_string()
        ))
    }

    #[test]
    fn test_type_check_function_2() {
        assert_debug_snapshot!(type_check_source(
            "
            let x = 2
            let f = 'hi'.x
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_type_check_function_3() {
        assert_debug_snapshot!(type_check_source(
            "
            let identity = \\x => x
            let x = 'hello'.identity
            let y: number = x
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_type_check_destructure_record() {
        assert_debug_snapshot!(type_check_source(
            "
            let f = \\{x, y = {z}} => {x, z}
            let x: {x: string, z: number} = {x = 'hi', y = {z = 3}}.f
            let y: {x: string, z: number} = {x = 2, y = {z = 3}}.f
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_infer_generic_function_1() {
        assert_debug_snapshot!(type_check_source(
            "
let identity = \\x => x
let constant = \\x => 2
let map = \\(x, f) => x.f
let x: string = 'hello'.map(identity)
let y: string = 'hello'.map(\\a => 'yo')
let z: string = 'hello'.map(constant)
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_unify_function_branches_1() {
        assert_debug_snapshot!(type_check_source(
            "
            let and = 
              \\(#a, #x) => #true
              \\(#b, #y) => #false
            let a = #a.and(#x)
            let b = #a.and(#y)
            let c = #b.and(#x)
            let d = #b.and(#y)
            let e = #bomb.and(#y)
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_unify_function_branches_2() {
        assert_debug_snapshot!(type_check_source(
            "
            type Boolean = #true | #false
            let and: \\(Boolean, Boolean) => Boolean = 
              \\(#bomb, #true) => #true
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_unify_function_branches_3() {
        // missing case
        assert_debug_snapshot!(type_check_source(
            "
            type Boolean = #true | #false
            let result = \\(x: Boolean) => x.(
                \\#true => 1
                \\#false => 'yo'
            )
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_number() {
        assert_debug_snapshot!(type_check_source(
            // expected number, got string at 'bomb'
            "
            let guess = \\x => 
                let 0 = x
                'bomb'
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_string() {
        assert_debug_snapshot!(type_check_source(
            // expected string, got number at 2
            "
            let guess = \\x => 
                let 'yo' = x
                2
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_null() {
        assert_debug_snapshot!(type_check_source(
            // expected string, got number at 2
            "
            let f = \\x => 
                let null = x
                2
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_boolean() {
        assert_debug_snapshot!(type_check_source(
            // expected string, got number at 2
            "
            let guess = \\x => 
                let true = x
                2
            "
            .trim()
            .to_string()
        ))
    }

    // #[test]
    // fn test_let_monadic_binding_array() {
    // }

    #[test]
    fn test_let_monadic_binding_tagged_union() {
        assert_debug_snapshot!(type_check_source(
            // expected union, got number at 2
            "
            let f = \\x => 
                let #some(yo) = x
                2
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_tagged_union_with_else_branch() {
        assert_debug_snapshot!(type_check_source(
            // expected union, got string at 'walao'
            "
            let f = \\x => 
                let #some(yo) = x else \\'walao' => 10
                2
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_generic_tagged_union_1() {
        assert_debug_snapshot!(type_check_source(
            " 
            type Option<T> = #some(T) | #none
            let add = \\(x: Option<number>) =>             
                let #some(y) = x
                y
            "
            .to_string()
        ))
    }

    #[test]
    fn test_pattern_match_generic_tagged_union() {
        assert_debug_snapshot!(type_check_source(
            " 
            type Option<T> = #some(T) | #none
            let add = \\(x: Option<number>) => x.(
                \\#some(x) => x
                \\#false => 2
            )
            "
            .to_string()
        ))
    }

    // #[test]
    // fn test_generic_function_with_specified_type_variable() {
    //     assert_debug_snapshot!(type_check_source(
    //         "
    //         let as<T> = \\(x: T): T => x
    //         let x = 'hello'.as<number>
    //         "
    //         .trim()
    //         .to_string()
    //     ))
    // }
    //

    #[test]
    fn array_homogeneous_element_1() {
        assert_debug_snapshot!(type_check_source("let x = [1, #hey]".trim().to_string()))
    }

    #[test]
    fn type_annotation_function_return_type() {
        assert_debug_snapshot!(type_check_source(
            "
         let f = \\(x): number => 'hi'
         "
            .to_string()
        ))
    }

    #[test]
    fn type_annotation_record_property_type() {
        assert_debug_snapshot!(type_check_source(
            "
         let x = {a: string = 2}
         "
            .to_string()
        ))
    }

    #[test]
    fn boolean_literal() {
        assert_debug_snapshot!(type_check_source(
            "
         let x: boolean = true
         let y: number = false
         "
            .to_string()
        ))
    }

    #[test]
    fn null_literal() {
        assert_debug_snapshot!(type_check_source(
            "
         let x: null = null
         let y: number = null
         "
            .to_string()
        ))
    }
}
