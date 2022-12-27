use crate::{
    compile::{compile, CompileError, CompileErrorKind, Source},
    formatter::ToDoc,
    parse_simple,
    stringify_error::print_compile_error,
    tokenize::Tokenizer,
};
use clap::Clap;
use std::{path::PathBuf, rc::Rc};

#[derive(Clap)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    /// Execute a KK script
    Run(Run),
    /// Format a KK script
    Format(Format),
}

#[derive(Clap)]
struct Run {
    /// Input filename
    filename: String,
}

#[derive(Clap)]
struct Format {
    /// Input filename
    filename: Option<String>,
}

pub fn cli() {
    let opts: Opts = Opts::parse();

    match opts.subcmd {
        SubCommand::Run(run) => compile(find_file(&run.filename)),
        SubCommand::Format(format) => {
            let (path, code) = match format.filename {
                Some(filename) => {
                    let path = find_file(&filename);
                    let code = std::fs::read_to_string(path.clone()).unwrap();
                    (path.to_str().unwrap().to_string(), code)
                }
                None => (
                    "<stdin>".to_string(),
                    std::io::stdin()
                        .lines()
                        .into_iter()
                        .map(|line| line.unwrap())
                        .collect::<Vec<_>>()
                        .join(""),
                ),
            };
            match parse_simple::Parser::parse(&mut Tokenizer::new(code.clone())) {
                Ok(ast) => {
                    println!("{}", ast.to_pretty())
                }
                Err(parse_error) => print_compile_error(CompileError {
                    kind: CompileErrorKind::ParseError(Box::new(parse_error)),
                    source: Rc::new(Source { path, code }),
                }),
            }
        }
    }
}

fn find_file<'a>(filename: &'a str) -> PathBuf {
    match PathBuf::from(filename).canonicalize() {
        Err(error) => {
            panic!("Unable to find file '{}'. Error = {}", filename, error);
        }
        Ok(path) => path,
    }
}
