use crate::{compile::compile, formatter::prettify_code};
use clap::Clap;
use std::path::PathBuf;

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
    filename: String,
}

pub fn cli() {
    let opts: Opts = Opts::parse();

    match opts.subcmd {
        SubCommand::Run(run) => match PathBuf::from(&run.filename).canonicalize() {
            Err(error) => {
                panic!("Unable to find file '{}'. Error = {}", run.filename, error);
            }
            Ok(path) => compile(path),
        },
        SubCommand::Format(format) => {
            todo!()
        }
    }
}
