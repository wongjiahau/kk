use crate::ast::*;
use crate::parse::Parser;
use crate::stringify_error::{print_parse_error, print_tokenize_error, print_unify_error};
use crate::tokenize::tokenize;
use crate::transpile_cps::transpile_statements;
use crate::unify::{unify_program, Program};

pub fn compile(source: Source, code: String) {
    match tokenize(code.clone()) {
        Err(tokenize_error) => print_tokenize_error(source, code, tokenize_error),
        Ok(tokens) => match Parser::parse(tokens) {
            Err(parse_error) => print_parse_error(source, code, parse_error),
            Ok(statements) => match unify_program(Program {
                source: source.clone(),
                statements,
            }) {
                Err(unify_error) => print_unify_error(source, code, unify_error),
                Ok(statements) => {
                    use std::process::Command;
                    let javascript = transpile_statements(statements);
                    // println!("{}", javascript);
                    let output = Command::new("node")
                        .arg("-e")
                        .arg(javascript)
                        .output()
                        .expect("Failed to run NodeJS binary");

                    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                    if !stdout.is_empty() {
                        println!("{}", stdout.trim())
                    }
                    if !stderr.is_empty() {
                        eprintln!("{}", stderr.trim())
                    }
                }
            },
        },
    }
}
