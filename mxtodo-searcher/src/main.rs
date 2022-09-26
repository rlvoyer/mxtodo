#[macro_use]
extern crate failure;

use clap::Parser;
use failure::Error;
use serde_json;

use mxtodo_searcher::{_search_directory, MxtodoSearcherError};

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

#[derive(Debug, Fail)]
enum MxtodoSearchError {
    #[fail(display = "error searching directory {}: {}", directory, error)]
    DirectorySearchError { directory: String, error: MxtodoSearcherError },
    #[fail(display = "error serializing search result to JSON: {}", error)]
    JsonSerializationError { error: serde_json::Error },
}

fn main() -> Result<(), Error> {
    let opts: Opts = Opts::parse();

    let pattern = opts.pattern.unwrap_or(DEFAULT_PATTERN.to_string());
    let file_ext = opts.file_ext.unwrap_or(DEFAULT_EXTENSION.to_string());
    let directory = opts.directory;

    let (todos, errors) = _search_directory(&directory, &file_ext, &pattern)
        .map_err(|error| MxtodoSearchError::DirectorySearchError { directory, error })?;

    if errors.len() > 0 {
        eprintln!("Encountered {} errors when searching for TODOs", errors.len());
        for error in errors {
            eprintln!("{}", error)
        }
    }
    
    for todo in todos {
        let todo_json = serde_json::to_string(&todo)
            .map_err(|error| MxtodoSearchError::JsonSerializationError { error })?;
        println!("{}", todo_json);
    }

    Ok(())
}
