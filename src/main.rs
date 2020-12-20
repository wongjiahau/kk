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

mod stringify_error;
use stringify_error::*;

#[test]
fn run_all_tests() {
    use difference::Changeset;
    use std::fs;
    let test_dir = "tests/compiler/";
    let dirs = fs::read_dir(test_dir).expect("Failed to read directory");
    for maybe_entry in dirs {
        let entry = maybe_entry.expect("Failed to read entry");
        let filename = entry
            .path()
            .to_str()
            .expect("Failed to convert entry to string")
            .to_string();

        if filename.ends_with(".kk") {
            let input_filename = filename;
            let input = fs::read_to_string(&input_filename).expect("failed to read input file");
            print!("{}", input_filename);
            let actual_output = compile(
                Source::File {
                    path: input_filename.clone(),
                },
                input.clone(),
            )
            .trim()
            .to_string();

            let actual_output = strip_line_trailing_spaces(actual_output);

            let stripped_actual_output = String::from_utf8(
                strip_ansi_escapes::strip(actual_output.clone()).expect("Failed to strip color"),
            )
            .unwrap();
            let output_filename = input_filename.clone() + ".out";
            let expected_output = fs::read_to_string(output_filename.as_str())
                .expect(format!("failed to read output file for {}", output_filename).as_str())
                .trim()
                .to_string();

            let expected_output = strip_line_trailing_spaces(expected_output);

            if stripped_actual_output.trim() != expected_output {
                let changeset = Changeset::new(&expected_output, &actual_output, "");
                println!("{}", "=".repeat(10));
                println!(
                    "ASSERTION FAILED FOR:\n\n{}",
                    indent_string(input_filename, 4)
                );
                println!("\n\nINPUT:\n\n{}", indent_string(input, 4));
                println!(
                    "\n\nEXPECTED OUTPUT:\n\n{}",
                    indent_string(expected_output, 4)
                );
                println!(
                    "\n\nACTUAL OUTPUT({}):\n\n{}",
                    output_filename,
                    indent_string(actual_output, 4)
                );
                println!(
                    "\n\nDIFF (EXPECTED OUTPUT / ACTUAL OUTPUT):\n\n{}",
                    indent_string(changeset.to_string(), 4)
                );
                println!("{}", "=".repeat(10));
                panic!()
            } else {
                println!(" PASSED");
            }
        }
    }
    fn strip_line_trailing_spaces(input: String) -> String {
        input
            .split('\n')
            .into_iter()
            .map(|line| line.trim_end().to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

fn main() {
    println!("{}", execute("let x = 2".to_string()));
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

pub fn compile(source: Source, code: String) -> String {
    match source_to_statements(code.clone()) {
        Err(parse_error) => format!("{:#?}", parse_error),
        Ok(statements) => match unify_program(Program {
            source: source.clone(),
            statements,
        }) {
            Err(unify_error) => stringify_unify_error(source, code, unify_error),
            Ok(_) => "".to_string(),
        },
    }
}

pub fn type_check_source(code: String) -> String {
    let source = Source::NonFile {
        env_name: "TEST".to_string(),
    };
    compile(source, code)
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

pub fn execute(source: String) -> String {
    let statements = source_to_statements(source).unwrap();
    // println!("{:#?}", statements);
    let javascript = transpile_statements(statements);
    use std::process::Command;
    let output = Command::new("node")
        .arg(format!("-e \"{}\"", javascript))
        .output()
        .expect("failed to execute process");
    vec![
        format!("status = {}", output.status),
        format!("stdout = {}", String::from_utf8_lossy(&output.stdout)),
        format!("stderr = {}", String::from_utf8_lossy(&output.stderr)),
    ]
    .join("\n")
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
                    line_end: 0,
                    character_index_start: 0,
                    character_index_end: 0
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
                        line_end: 0,
                        character_index_start: 0,
                        character_index_end: 0
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
                        character_index_start: 0,
                        character_index_end: 0
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
                    line_end: 0,
                    character_index_start: 0,
                    character_index_end: 0
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
                    line_end: 0,
                    character_index_start: 0,
                    character_index_end: 0
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
                        column_end: 2,
                        character_index_start: 0,
                        character_index_end: 0
                    }
                },
                Token {
                    token_type: TokenType::Whitespace,
                    representation: " ".to_string(),
                    position: Position {
                        line_start: 0,
                        line_end: 0,
                        column_start: 3,
                        column_end: 3,
                        character_index_start: 0,
                        character_index_end: 0
                    }
                },
                Token {
                    token_type: TokenType::KeywordType,
                    representation: "type".to_string(),
                    position: Position {
                        line_start: 0,
                        line_end: 0,
                        column_start: 4,
                        column_end: 7,
                        character_index_start: 0,
                        character_index_end: 0
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

    #[test]
    fn transpiled_destructured_array_1() {
        assert_debug_snapshot!(transpile_source(
            "let head = \\xs => 
                let [head, ...] = xs else \\_ => #none
                #some(head)
            "
            .to_string()
        ))
    }

    #[test]
    fn transpiled_destructured_array_2() {
        assert_debug_snapshot!(transpile_source(
            "let tail = \\xs => 
                let [_, ...tail] = xs
                tail
            "
            .to_string()
        ))
    }

    #[test]
    fn transpiled_destructured_array_3() {
        assert_debug_snapshot!(transpile_source(
            "let init = \\xs => 
                let [...init, _] = xs
                init
            "
            .to_string()
        ))
    }

    #[test]
    fn transpiled_destructured_array_4() {
        assert_debug_snapshot!(transpile_source(
            "let last = \\xs => 
                let [..., last] = xs else \\_ => #none
                #some(last)
            "
            .to_string()
        ))
    }

    #[test]
    fn transpiled_destructured_array_5() {
        assert_debug_snapshot!(transpile_source(
            "let arrayEmpty = 
                \\[] => true
                \\_ => false
            "
            .to_string()
        ))
    }

    #[test]
    fn transpiled_destructured_array_6() {
        assert_debug_snapshot!(transpile_source(
            "let arrayOnlyOneElement = 
                \\[_] => true
                \\_ => false
            "
            .to_string()
        ))
    }

    #[test]
    fn transpiled_destructured_array_7() {
        // expected error, cannot have more than one spread
        assert_debug_snapshot!(transpile_source(
            "let arrayOnlyOneElement = 
                \\[a,...b,...c,d] => true
                \\_ => false
            "
            .to_string()
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

    #[test]
    fn test_let_monadic_binding_array() {
        assert_debug_snapshot!(type_check_source(
            "
            let f = \\x =>
              let [a, ...b, c] = x
              let y: string = b
              1
        "
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_tagged_union_1() {
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
    fn test_let_monadic_binding_tagged_union_with_non_exhaustive_else_branch() {
        assert_debug_snapshot!(type_check_source(
            // missing case "#blue" case at else branch
            "
            type Color = #red | #green | #blue
            let colorIsRed = \\(color: Color) => 
                let #red = color 
                  else 
                    \\#green => false
                true
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_variable() {
        assert_debug_snapshot!(type_check_source(
            // expect no error
            "
            let colorIsRed = \\_ => 
                let x = 1
                let y = 'hello'
                []
            "
            .trim()
            .to_string()
        ))
    }

    #[test]
    fn test_let_monadic_binding_variable_2() {
        assert_debug_snapshot!(type_check_source(
            // unexpected else branch
            "
            let colorIsRed = \\_ => 
                let x = 1 
                  else 
                    \\_ => 'hi'
                []
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

    #[test]
    fn type_inference_infinite_type() {
        // expected error
        assert_debug_snapshot!(type_check_source(
            "
            let f = \\x => x.x
         "
            .to_string()
        ))
    }
}
