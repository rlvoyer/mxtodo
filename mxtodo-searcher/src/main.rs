#[macro_use]
extern crate failure;

use clap::{AppSettings, Clap};
use failure::Error;
use serde_json;

use mxtodo_searcher::_search_directory;

const DEFAULT_PATTERN: &str = r"^- ?\[[Xx ]\] ";
const DEFAULT_EXTENSION: &str = r".md";

/// Search for TODOs in a specified directory.
/// Optionally specify a regex pattern and a file extension for files that contain TODOs.
#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
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
    DirectorySearchError { directory: String, error: Error },
    #[fail(display = "error serializing search result to JSON: {}", error)]
    JsonSerializationError { error: serde_json::Error },
}

fn main() -> Result<(), Error> {
    let opts: Opts = Opts::parse();

    let pattern = opts.pattern.unwrap_or(DEFAULT_PATTERN.to_string());
    let file_ext = opts.file_ext.unwrap_or(DEFAULT_EXTENSION.to_string());
    let directory = opts.directory;

    let results = _search_directory(directory.clone(), file_ext, pattern)
        .map_err(|error| MxtodoSearchError::DirectorySearchError { directory, error })?;

    for search_result in results {
        let search_result_json = serde_json::to_string(&search_result)
            .map_err(|error| MxtodoSearchError::JsonSerializationError { error })?;
        println!("{}", search_result_json);
    }

    Ok(())
}
