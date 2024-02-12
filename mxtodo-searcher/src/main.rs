extern crate failure;

use clap::Parser;
use failure::Error;
use serde_json;

use mxtodo_searcher::{MxtodoSearcherError, _search_directory};

const DEFAULT_PATTERN: &str = r"^- ?\[[Xx ]\] ";
const DEFAULT_EXTENSION: &str = r".md";

/// Search for TODOs in a specified directory.
/// Optionally specify a regex pattern and a file extension for files that contain TODOs.
#[derive(Parser)]
#[clap()]
struct Opts {
    /// The directory in which to search for TODOs.
    directory: String,
    /// The TODO pattern to search for. If unspecified, defaults to `DEFAULT`.
    pattern: Option<String>,
    /// The file extension for note files in which to search for TODOs.
    file_ext: Option<String>,
}

fn main() -> Result<(), Error> {
    let opts: Opts = Opts::parse();

    let pattern = opts.pattern.unwrap_or(DEFAULT_PATTERN.to_string());
    let file_ext = opts.file_ext.unwrap_or(DEFAULT_EXTENSION.to_string());
    let directory = opts.directory;

    let (todos, errors) = _search_directory(&directory, &file_ext, &pattern)?;

    if errors.len() > 0 {
        eprintln!("Encountered {} errors when searching for TODOs", errors.len());
        for error in errors {
            eprintln!("{}", error)
        }
    }

    for todo in todos {
        let todo_json = serde_json::to_string(&todo).map_err(|error| {
            MxtodoSearcherError::JsonSerializationError(format!(
                "unable to JSON serialize the TODO: {:?}",
                error.to_string()
            ))
        })?;
        println!("{}", todo_json);
    }

    Ok(())
}
