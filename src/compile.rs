use crate::ast::*;
use crate::parse::Parser;
use crate::stringify_error::print_unify_error;
use crate::tokenize::tokenize;
use crate::transpile::transpile_statements;
use crate::unify::{unify_program, Program};

pub fn compile(source: Source, code: String) {
    match tokenize(code.clone()) {
        Err(tokenize_error) => {
            panic!("not implemented")
        }
        Ok(tokens) => match Parser::parse(tokens) {
            Err(parse_error) => {
                panic!()
            }
            Ok(statements) => match unify_program(Program {
                source: source.clone(),
                statements: statements.clone(),
            }) {
                Err(unify_error) => print_unify_error(source, code, unify_error),
                Ok(_) => {
                    use std::process::Command;
                    let javascript = transpile_statements(statements);
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
