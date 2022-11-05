use std::{env, path::PathBuf};

/// Converts an absolute path to a path relative to the current working directory.
pub fn to_relative_path(path: PathBuf) -> PathBuf {
    path.strip_prefix(env::current_dir().unwrap())
        .unwrap()
        .to_path_buf()
}
