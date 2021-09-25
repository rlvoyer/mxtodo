#[macro_use]
extern crate failure;

use std::ffi::OsString;
use std::io;

use emacs::{defun, Env, Value};
use failure::Error;
use grep::{regex::RegexMatcherBuilder, searcher::BinaryDetection, searcher::SearcherBuilder};
use serde::Serialize;
use walkdir::WalkDir;

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
//#[emacs::module(name="mxtodo-searcher", defun_prefix="mxtodo", mod_in_name=false)]
#[emacs::module]
fn init(env: &Env) -> emacs::Result<Value<'_>> {
    env.message("Done loading!")
}

fn files_to_search(directory: String, file_ext: String) -> Vec<OsString> {
    WalkDir::new(&directory)
        .min_depth(1)
        .follow_links(true)
        .into_iter()
        .filter_entry(|dir_entry| {
            let file_name = dir_entry.file_name().to_string_lossy();
            file_name.ends_with(&file_ext) && dir_entry.file_type().is_file()
        })
        .filter_map(|dir_entry| dir_entry.ok())
        .map(|dir_entry| dir_entry.into_path().into_os_string())
        .collect()
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct TodoMatch {
    pub file_path: String,
    pub line_number: u64,
    pub line: String,
}

impl TodoMatch {
    fn new(file_path: OsString, line_number: u64, line: &str) -> TodoMatch {
        TodoMatch {
            file_path: file_path.to_string_lossy().to_string(),
            line_number,
            line: line.trim_end_matches(&['\r', '\n'][..]).to_string(),
        }
    }
}

pub fn _search_directory(
    directory: String,
    file_ext: String,
    pattern: String,
) -> Result<Vec<TodoMatch>, Error> {
    let matcher = RegexMatcherBuilder::new()
        .multi_line(true)
        .line_terminator(Some(b'\n'))
        .build(&pattern)?;

    let mut searcher = SearcherBuilder::new()
        .binary_detection(BinaryDetection::quit(b'\x00'))
        .line_number(true)
        .build();

    let found = files_to_search(directory, file_ext)
        .into_iter()
        .map(|file_path| {
            let mut results: Vec<TodoMatch> = vec![];

            let capture_results = |line_number: u64, line: &str| -> Result<bool, io::Error> {
                let todo_match = TodoMatch::new(file_path.clone(), line_number, line);
                results.push(todo_match);
                Ok(true)
            };

            let results_sink = grep::searcher::sinks::UTF8(capture_results);

            searcher.search_path(&matcher, &file_path, results_sink)?;

            Ok(results)
        })
        .collect::<Result<Vec<Vec<TodoMatch>>, Error>>()?;

    Ok(found.into_iter().flatten().collect())
}

/// This searches `directory` for files with extension `file_ext` returning a list of todos as 2-element vectors.
#[defun]
fn search_directory(
    env: &Env,
    directory: String,
    file_ext: String,
    pattern: String,
) -> emacs::Result<Value> {
    let found = _search_directory(directory, file_ext, pattern)?;

    let elisp_results = found
        .into_iter()
        .map(|todo| env.vector((todo.file_path, todo.line_number, todo.line)))
        .collect::<emacs::Result<Vec<Value>>>()?;

    env.vector(&elisp_results)
}

#[cfg(test)]
mod tests {
    use crate::*;
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn files_to_search_returns_the_expected_files() -> Result<(), String> {
        let dir =
            tempdir().map_err(|e| format!("Unable to create a temporary directory: {:?}", e))?;
        let file_path = dir.path().join("my-temporary-file.md");
        let mut file1 = File::create(&file_path)
            .map_err(|e| format!("Unable to create a temp file: {:?}", e))?;
        writeln!(file1, "Test data.")
            .map_err(|e| format!("Unable to write to temp file: {:?}", e))?;

        let expected = vec![file_path.into_os_string()];
        let actual = files_to_search(
            dir.into_path()
                .into_os_string()
                .to_string_lossy()
                .to_string(),
            ".md".to_string(),
        );
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn files_to_search_returns_no_files() -> Result<(), String> {
        let dir =
            tempdir().map_err(|e| format!("Unable to create a temporary directory: {:?}", e))?;
        let file_path = dir.path().join("my-temporary-file.derp");
        let mut file1 = File::create(&file_path)
            .map_err(|e| format!("Unable to create a temp file: {:?}", e))?;
        writeln!(file1, "Test data.")
            .map_err(|e| format!("Unable to write to temp file: {:?}", e))?;

        let expected: Vec<OsString> = vec![];
        let actual = files_to_search(
            dir.into_path()
                .into_os_string()
                .to_string_lossy()
                .to_string(),
            ".md".to_string(),
        );
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn search_directory_returns_the_expected_matches() -> Result<(), String> {
        let dir =
            tempdir().map_err(|e| format!("Unable to create a temporary directory: {:?}", e))?;
        let file_path: std::path::PathBuf = dir.path().join("my-todo-file.md");
        let mut file1 = File::create(&file_path)
            .map_err(|e| format!("Unable to create a temp file: {:?}", e))?;
        writeln!(file1, "- [ ] do your taxes")
            .map_err(|e| format!("Unable to write to temp file: {:?}", e))?;

        let pattern = r"^- ?\[[Xx ]\] ".to_string();
        let expected: Vec<TodoMatch> = vec![TodoMatch::new(
            file_path.into_os_string(),
            1,
            "- [ ] do your taxes",
        )];
        let actual = _search_directory(
            dir.into_path()
                .into_os_string()
                .to_string_lossy()
                .to_string(),
            ".md".to_string(),
            pattern,
        )
        .unwrap();

        assert_eq!(expected, actual);

        Ok(())
    }
}
