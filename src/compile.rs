use crate::ast::*;
use crate::parse::parse_statements;
use crate::stringify_error::print_unify_error;
use crate::tokenize::tokenize;
use crate::unify::{unify_program, Program};

pub fn compile(source: Source, code: String) {
    match source_to_statements(code.clone()) {
        Err(parse_error) => {
            println!("{:#?}", parse_error);
        }
        Ok(statements) => match unify_program(Program {
            source: source.clone(),
            statements,
        }) {
            Err(unify_error) => print_unify_error(source, code, unify_error),
            Ok(_) => {
                // TODO: transpile and execute the Javascript
            }
        },
    }
}

pub fn source_to_statements(source: String) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokenize(source)?;
    parse_statements(tokens)
}
