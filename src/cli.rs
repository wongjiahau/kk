use crate::compile::compile;
use crate::{ast::Source, unify::Program};
use clap::Clap;
use std::fs;

#[derive(Clap)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    /// Execute a KK script
    Run(Run),
}

#[derive(Clap)]
struct Run {
    /// Input filename
    filename: String,
}

pub fn cli() {
    let opts: Opts = Opts::parse();

    match opts.subcmd {
        SubCommand::Run(run) => {
            match fs::read_to_string(&run.filename) {
                Err(_) => {
                    eprintln!("Unable to find file '{}'", run.filename);
                }
                Ok(code) => compile(Program {
                    source: Source::File { path: run.filename },
                    code,
                    import_relations: vec![], // empty, because this is the root
                }),
            }
        }
    }
}
