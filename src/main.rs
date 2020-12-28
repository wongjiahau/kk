pub mod ast;
use ast::*;

mod environment;

mod unify;
use unify::*;

mod transpile;
use transpile::*;

mod parse;

mod tokenize;
use tokenize::*;

mod stringify_error;
use stringify_error::*;

mod cli;
use cli::*;

mod compile;
use compile::*;

mod pattern;

#[test]
fn run_all_tests() {
    use colored::*;
    use insta::assert_snapshot;
    use std::fs;
    use std::process::Command;

    let test_dir = "tests/compiler/";
    let folders = fs::read_dir(test_dir).expect("Failed to read directory");
    for folder in folders {
        let files = fs::read_dir(folder.unwrap().path().to_str().unwrap())
            .expect("Failed to read directory");
        for file in files {
            let file = file.expect("Failed to read entry");
            let filename = file
                .path()
                .to_str()
                .expect("Failed to convert entry to string")
                .to_string();

            if filename.ends_with(".kk") {
                let input_filename = filename;
                let input = fs::read_to_string(&input_filename).expect("failed to read input file");
                print!("{}", input_filename);
                let actual_output = {
                    let output = Command::new("./target/debug/kk")
                        .arg("run")
                        .arg(&input_filename)
                        .output()
                        .expect("Failed to run KK CLI");

                    let exit_code = match output.status.code() {
                        Some(code) => code.to_string(),
                        None => "".to_string(),
                    };

                    vec![
                        "============".to_string(),
                        "INPUT".to_string(),
                        "============".to_string(),
                        input.trim().to_string(),
                        "".to_string(),
                        "============".to_string(),
                        "EXIT CODE".to_string(),
                        "============".to_string(),
                        exit_code,
                        "".to_string(),
                        "============".to_string(),
                        "STDOUT".to_string(),
                        "============".to_string(),
                        strip_line_trailing_spaces(
                            String::from_utf8_lossy(&output.stdout.clone()).to_string(),
                        )
                        .trim()
                        .to_string(),
                        "".to_string(),
                        "============".to_string(),
                        "STDERR".to_string(),
                        "============".to_string(),
                        strip_line_trailing_spaces(
                            String::from_utf8_lossy(&output.stderr.clone()).to_string(),
                        )
                        .trim()
                        .to_string(),
                    ]
                    .join("\n")
                };

                let actual_output = strip_line_trailing_spaces(actual_output);

                let stripped_actual_output = strip_line_trailing_spaces(
                    String::from_utf8(
                        strip_ansi_escapes::strip(actual_output.clone())
                            .expect("Failed to strip color"),
                    )
                    .unwrap(),
                );

                assert_snapshot!(input_filename.clone(), stripped_actual_output.trim());
                println!("{}", " PASSED".green());

                // if stripped_actual_output.trim() != expected_output {
                //     let changeset = Changeset::new(&expected_output, &stripped_actual_output, "");
                //     println!("{}", "=".repeat(10));
                //     println!(
                //         "ASSERTION FAILED FOR:\n\n{}",
                //         indent_string(input_filename.to_string(), 4)
                //     );
                //     println!("\n\nINPUT:\n\n{}", indent_string(input, 4));
                //     println!(
                //         "\n\nEXPECTED OUTPUT:\n\n{}",
                //         indent_string(expected_output, 4)
                //     );
                //     println!(
                //         "\n\nACTUAL OUTPUT({}):\n\n{}",
                //         output_filename,
                //         indent_string(actual_output, 4)
                //     );
                //     println!(
                //         "\n\nDIFF (EXPECTED OUTPUT / ACTUAL OUTPUT):\n\n{}",
                //         indent_string(changeset.to_string(), 4)
                //     );
                //     println!("{}", "=".repeat(10));
                //     panic!("ASSERTION FAILED.")
                // } else {
                //     println!("{}", " PASSED".green());
                // }
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
    cli();
}

#[derive(Debug)]
pub enum CompileError {
    UnifyError(UnifyError),
    ParseError(ParseError),
}

pub fn type_check_source(code: String) -> String {
    let source = Source::NonFile {
        env_name: "TEST".to_string(),
    };
    compile(source, code);
    panic!()
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
}
