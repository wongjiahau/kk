use crate::compile::compile;
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
}

#[derive(Clap)]
struct Run {
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
    }
}
