use std::path::PathBuf;
use std::rc::Rc;

use include_dir::{include_dir, Dir};
use indexmap::IndexMap;

use crate::interpret::interpret_statements;
use crate::module::fs_readdir_to_directory;
use crate::module::ModuleMeta;
use crate::module::ModuleUid;
use crate::parse::ParseError;
use crate::stringify_error::print_compile_error;
use crate::transpile::interpretable::print_statements;
use crate::transpile::transpile_program;
use crate::unify::read_module;
use crate::unify::UnifyError;
use crate::utils::to_relative_path;

#[derive(Debug)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub source: Rc<Source>,
}

#[derive(Debug)]
pub struct Source {
    pub path: String,
    pub code: String,
}

#[derive(Debug)]
pub enum CompileErrorKind {
    ParseError(Box<ParseError>),
    UnifyError(Box<UnifyError>),
}

pub static STDLIB_DIR: Dir<'_> = include_dir!("./stdlib");
pub fn compile(path: PathBuf) {
    let folder_relative_path = to_relative_path(path.parent().unwrap().to_path_buf()).unwrap();
    let uid = ModuleUid::Local {
        folder_relative_path: folder_relative_path.clone(),
    };
    let module_meta = ModuleMeta {
        uid: uid.clone(),
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
            uid,
            fs_readdir_to_directory(dir),
            Some(&to_relative_path(path).unwrap()),
        ) {
            Err(compile_error) => print_compile_error(compile_error),
            Ok(result) => {
                // println!("result = {:#?}", result);
                let ast = transpile_program(result);
                // println!("ast = {}", print_statements(ast.clone()));
                interpret_statements(ast);
            }
        },
    }
}
