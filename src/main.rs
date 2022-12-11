mod inferred_ast;
pub mod raw_ast;

mod module;
mod utils;

mod unify;

mod transpile;
// mod transpile_cps;

mod parse;

mod tokenize;

mod stringify_error;

mod cli;
mod formatter;
mod innate_function;
mod interpret;
mod solved_ast;
mod typ;
use cli::*;

mod compile;

mod pattern;

mod non_empty;

fn main() {
    cli();
}
