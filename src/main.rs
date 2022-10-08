mod inferred_ast;
mod raw_ast;
mod simple_ast;

mod module;

mod unify;

mod transpile;
// mod transpile_cps;

mod parse;

mod tokenize;

mod stringify_error;

mod cli;
mod partially_qualified_ast;
mod qualified_ast;
mod solved_ast;
mod typ;
use cli::*;

mod compile;

mod pattern;

mod non_empty;

fn main() {
    cli();
}
