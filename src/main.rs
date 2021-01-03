pub mod ast;

mod environment;

mod unify;

mod transpile;

mod parse;

mod tokenize;

mod stringify_error;

mod cli;
use cli::*;

mod compile;

mod pattern;

fn main() {
    cli();
}
