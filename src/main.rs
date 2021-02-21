pub mod ast;
mod typechecked_ast;

mod module;

mod unify;

// mod transpile;
mod transpile_cps;

mod parse;

mod tokenize;

mod stringify_error;

mod cli;
use cli::*;

mod compile;

mod pattern;

mod non_empty;

fn main() {
    cli();
}
