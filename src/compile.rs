use std::env;
use std::path::PathBuf;
use std::process;

use indexmap::IndexMap;

use crate::module::ModuleMeta;
use crate::module::ModuleUid;
use crate::parse::ParseError;
use crate::stringify_error::print_compile_error;
use crate::transpile::transpile_program;
use crate::unify::read_module;
use crate::unify::UnifyError;
use crate::utils::to_relative_path;

#[derive(Debug)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub path: PathBuf,
}

#[derive(Debug)]
pub enum CompileErrorKind {
    ParseError(Box<ParseError>),
    UnifyError(Box<UnifyError>),
}

pub fn compile(path: PathBuf) {
    let folder_relative_path = to_relative_path(path.parent().unwrap().to_path_buf()).unwrap();
    let module_meta = ModuleMeta {
        uid: ModuleUid::Local {
            folder_relative_path: folder_relative_path.to_str().unwrap().to_string().clone(),
        },
        import_relations: vec![], // empty, because this is the root
    };
    match folder_relative_path.read_dir() {
        Err(error) => panic!(
            "Unable to read folder: {}, due to error: {}",
            folder_relative_path.display(),
            error
        ),
        Ok(dir) => match read_module(
            &module_meta,
            &IndexMap::new(),
            folder_relative_path.to_path_buf(),
            dir,
            Some(&to_relative_path(path).unwrap()),
        ) {
            Err(compile_error) => print_compile_error(compile_error),
            Ok(result) => {
                // result.interpret(result.into());
                use std::process::Command;
                let javascript = transpile_program(result);
                // println!("{}", javascript);
                let output = Command::new("node")
                    .arg("-e")
                    .arg(javascript)
                    .output()
                    .expect("Failed to run NodeJS binary");

                let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                if !stdout.is_empty() {
                    println!("{}", stdout.trim())
                }
                if !stderr.is_empty() {
                    eprintln!("{}", stderr.trim())
                }
                if let Some(code) = output.status.code() {
                    process::exit(code)
                }
            }
        },
    }
}
