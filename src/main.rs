mod inferred_ast;
pub mod raw_ast;

mod module;

mod unify;

mod transpile;
// mod transpile_cps;

mod parse;

mod tokenize;

mod stringify_error;

mod cli;
mod ibx;
mod javascript_ast;
mod solved_ast;
mod tail_call_optimisation;
mod typ;
use cli::*;

mod compile;

mod pattern;

mod non_empty;

fn main() {
    cli();
}
