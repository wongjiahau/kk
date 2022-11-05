use std::{env, path::PathBuf};

/// Converts an absolute path to a path relative to the current working directory.
pub fn to_relative_path(path: PathBuf) -> Result<PathBuf, String> {
    Ok(path
        .strip_prefix(env::current_dir().map_err(|error| error.to_string())?)
        .map_err(|error| error.to_string())?
        .to_path_buf())
}
