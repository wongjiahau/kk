use crate::stringify_error::{print_parse_error, print_tokenize_error};
use crate::tokenize::tokenize;
use crate::transpile_cps::transpile_statements;
use crate::unify::{unify_statements, Program};
use crate::{parse::*, stringify_error::print_compile_error};
use crate::{tokenize::TokenizeError, unify::UnifyError};

pub struct CompileError {
    pub kind: CompileErrorKind,
    pub program: Program,
}
pub enum CompileErrorKind {
    TokenizeError(TokenizeError),
    ParseError(Box<ParseError>),
    UnifyError(Box<UnifyError>),
}

pub fn compile(program: Program) {
    match tokenize(program.code.clone()) {
        Err(tokenize_error) => print_tokenize_error(program, tokenize_error),
        Ok(tokens) => match Parser::parse(tokens) {
            Err(parse_error) => print_parse_error(program, parse_error),
            Ok(statements) => match unify_statements(program, statements) {
                Err(compile_error) => print_compile_error(compile_error),
                Ok(result) => {
                    use std::process::Command;
                    let javascript = transpile_statements(result.statements);
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
