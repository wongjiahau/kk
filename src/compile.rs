use crate::ast::*;
use crate::parse::parse_statements;
use crate::stringify_error::print_unify_error;
use crate::tokenize::tokenize;
use crate::transpile::transpile_statements;
use crate::unify::{unify_program, Program};

pub fn compile(source: Source, code: String) {
    match source_to_statements(code.clone()) {
        Err(parse_error) => {
            println!("{:#?}", parse_error);
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
    }
}

pub fn source_to_statements(source: String) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokenize(source)?;
    parse_statements(tokens)
}
