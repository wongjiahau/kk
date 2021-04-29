use std::process;

use indexmap::IndexMap;

use crate::tokenize::Tokenizer;
use crate::transpile::transpile_program;
use crate::unify::unify_statements;
use crate::unify::UnifyError;
use crate::{module::ModuleMeta, stringify_error::print_parse_error};
use crate::{parse::*, stringify_error::print_compile_error};

pub struct CompileError {
    pub kind: CompileErrorKind,
    pub module_meta: ModuleMeta,
}
pub enum CompileErrorKind {
    ParseError(Box<ParseError>),
    UnifyError(Box<UnifyError>),
}

pub fn compile(module_meta: ModuleMeta) {
    let mut tokenizer = Tokenizer::new(module_meta.code.clone());
    match Parser::parse(&mut tokenizer) {
        Err(parse_error) => print_parse_error(module_meta, parse_error),
        Ok(statements) => {
            match unify_statements(module_meta, statements, &IndexMap::new(), true) {
                Err(compile_error) => print_compile_error(compile_error),
                Ok(result) => {
                    use std::process::Command;
                    let javascript = transpile_program(result);
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
                    if let Some(code) = output.status.code() {
                        process::exit(code)
                    }
                }
            }
        }
    }
}
