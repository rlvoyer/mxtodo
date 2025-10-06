#[macro_use]
extern crate comp;

#[macro_use]
extern crate failure;

use std::ffi::OsString;
use std::io;
use std::path::Path;

use chrono::serde::{ts_seconds, ts_seconds_option};
use chrono::{offset::TimeZone, DateTime, LocalResult, NaiveDate, NaiveDateTime, Utc};
use emacs::{defun, Env, IntoLisp, Value};
use grep::{regex::RegexMatcherBuilder, searcher::BinaryDetection, searcher::SearcherBuilder};
use itertools::{Either, Itertools};
use lazy_static::lazy_static;
use regex::Regex;
use serde::Serialize;
use walkdir::WalkDir;

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> emacs::Result<Value<'_>> {
    env.message("Done loading!")
}

fn files_to_search(directory: &str, file_ext: &str) -> Vec<OsString> {
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
pub struct Link {
    pub start_offset: usize,
    pub text_start_offset: usize,
    pub url_start_offset: usize,
    pub length: usize,
    pub text: String,
    pub url: String,
}

impl IntoLisp<'_> for Link {
    fn into_lisp(self, env: &Env) -> emacs::Result<Value<'_>> {
        let keys = env.call(
            "list",
            (
                "start_offset",
                "length",
                "text_start_offset",
                "text",
                "url_start_offset",
                "url",
            ),
        )?;

        let values = env.call(
            "list",
            (
                self.start_offset,
                self.length,
                self.text_start_offset,
                self.text,
                self.url_start_offset,
                self.url,
            ),
        )?;

        env.call(
            "cl-pairlis",
            &[keys.into_lisp(env)?, values.into_lisp(env)?],
        )
    }
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

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct Tag {
    pub start_offset: usize,
    pub length: usize,
    pub text: String,
}

impl IntoLisp<'_> for Tag {
    fn into_lisp(self, env: &Env) -> emacs::Result<Value<'_>> {
        let keys = env.call("list", ("start_offset", "length", "text"))?;

        let values = env.call("list", (self.start_offset, self.length, self.text))?;

        env.call(
            "cl-pairlis",
            &[keys.into_lisp(env)?, values.into_lisp(env)?],
        )
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize)]
pub struct Todo {
    file_path: String,
    line_number: u64,
    #[serde(with = "ts_seconds")]
    display_date: DateTime<Utc>,
    #[serde(with = "ts_seconds_option")]
    date_due: Option<DateTime<Utc>>,
    #[serde(with = "ts_seconds_option")]
    date_completed: Option<DateTime<Utc>>,
    text: String,
    is_completed: bool,
    links: Vec<Link>,
    tags: Vec<Tag>,
}

impl Todo {
    pub fn new(
        file_path: String,
        line_number: u64,
        display_date: DateTime<Utc>,
        date_due: Option<DateTime<Utc>>,
        date_completed: Option<DateTime<Utc>>,
        text: String,
        is_completed: bool,
        links: Vec<Link>,
        tags: Vec<Tag>,
    ) -> Todo {
        Todo {
            file_path,
            line_number,
            display_date,
            date_due,
            date_completed,
            text,
            is_completed,
            links,
            tags,
        }
    }
}

fn vec_to_lisp<'e, T: IntoLisp<'e>>(env: &'e Env, vec: Vec<T>) -> emacs::Result<Value> {
    let values: Vec<Value> = vec.into_iter().flat_map(|l| l.into_lisp(env)).collect();
    env.list(&values)
}

impl IntoLisp<'_> for Todo {
    fn into_lisp(self, env: &Env) -> emacs::Result<Value<'_>> {
        let keys = env.call(
            "list",
            (
                "file_path",
                "line_number",
                "display_date",
                "date_due",
                "date_completed",
                "text",
                "is_completed",
                "links",
                "tags",
            ),
        )?;

        let values = env.call(
            "list",
            (
                self.file_path,
                self.line_number,
                self.display_date.timestamp(),
                self.date_due.map(|d| d.timestamp()),
                self.date_completed.map(|d| d.timestamp()),
                self.text,
                self.is_completed,
                vec_to_lisp(env, self.links)?,
                vec_to_lisp(env, self.tags)?,
            ),
        )?;

        env.call(
            "cl-pairlis",
            &[keys.into_lisp(env)?, values.into_lisp(env)?],
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct TodoInfo {
    is_completed: bool,
    text: String,
    date_due: Option<DateTime<Utc>>,
    date_completed: Option<DateTime<Utc>>,
    links: Vec<Link>,
    tags: Vec<Tag>,
}

#[derive(Debug, Fail, PartialEq, Serialize)]
pub enum MxtodoSearcherError {
    #[fail(display = "error serializing search result to JSON: {}", _0)]
    JsonSerializationError(String),
    #[fail(display = "Malformed date string: {:?}", _0)]
    MalformedDateString(String),
    #[fail(display = "Malformed regular expression: {:?}", _0)]
    MalformedRegexPattern(String),
    #[fail(display = "A search error occurred: {:?}", _0)]
    SearchError(String),
    #[fail(display = "An IO error occurred: {:?}", _0)]
    UnexpectedIOError(String),
    #[fail(
        display = "An error occurred converting the Rust object in an elisp object: {:?}",
        _0
    )]
    UnexpectedRustToLispConversionError(String),
    #[fail(display = "Unable to parse the TODO line: {:?}", _0)]
    TodoParsingError(String),
    #[fail(display = "The specified directory does not exist: {:?}", _0)]
    DirectoryNotFound(String),
    #[fail(
        display = "The specified path is a file but should be a directory: {:?}",
        _0
    )]
    InvalidPathParameter(String),
}

fn datetime_from_ymd_str(ymd_str: &str) -> Result<DateTime<Utc>, MxtodoSearcherError> {
    let date = NaiveDate::parse_from_str(ymd_str, "%Y-%m-%d")
        .map_err(|_| MxtodoSearcherError::MalformedDateString(ymd_str.to_string()))?;

    let datetime = date
        .and_hms_opt(0, 0, 0)
        .ok_or(MxtodoSearcherError::MalformedDateString(
            ymd_str.to_string(),
        ))?;

    match Utc.from_local_datetime(&datetime) {
        LocalResult::Single(dt) => Ok(dt),
        LocalResult::Ambiguous(_, _) => Err(MxtodoSearcherError::MalformedDateString(format!(
            "Ambiguous datetime for {}",
            ymd_str
        ))),
        LocalResult::None => Err(MxtodoSearcherError::MalformedDateString(format!(
            "No datetime found for {}",
            ymd_str
        ))),
    }
}

fn utc_datetime_from_date_str_with_optional_time(
    date_str: &str,
) -> Result<DateTime<Utc>, MxtodoSearcherError> {
    let mut r: Result<NaiveDateTime, MxtodoSearcherError> =
        NaiveDateTime::parse_from_str(date_str, "%Y-%m-%dT%H:%M:%SZ")
            .map_err(|_| MxtodoSearcherError::MalformedDateString(date_str.to_string()));

    if r.is_err() {
        r = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
            .map_err(|_| MxtodoSearcherError::MalformedDateString(date_str.to_string()))
            .map(|dt| {
                dt.and_hms_opt(0, 0, 0)
                    .ok_or(MxtodoSearcherError::MalformedDateString(
                        date_str.to_string(),
                    ))
            })?;
    }

    r.map(|datetime| Utc.from_utc_datetime(&datetime))
}

fn display_date_from_file_path(file_path: &str) -> Result<DateTime<Utc>, MxtodoSearcherError> {
    let file_stem = Path::new(file_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or(MxtodoSearcherError::UnexpectedIOError(
            file_path.to_string(),
        ))?;

    datetime_from_ymd_str(file_stem)
}

fn extract_links(todo_text: &str) -> Vec<Link> {
    lazy_static! {
        static ref MARKDOWN_LINK: Regex =
            Regex::new(r"\[(?P<link_text>[^\]]+)\]\((?P<link_url>[^\)]+)\)").unwrap();
    }

    let links: Vec<Link> = MARKDOWN_LINK
        .captures_iter(todo_text)
        .flat_map(|capture| {
            let text_and_start_offset = capture
                .name("link_text")
                .map(|t| (t.as_str().to_string(), t.start()));

            let url_and_start_offset = capture
                .name("link_url")
                .map(|u| (u.as_str().to_string(), u.start()));

            option! {
                let match_ <- capture.get(0);
                let start_offset = match_.start();
                let length = match_.end() - start_offset;

                let (text, text_start_offset) <- text_and_start_offset;
                let (url, url_start_offset) <- url_and_start_offset;

                Link {
                    start_offset,
                    text_start_offset,
                    url_start_offset,
                    length,
                    text,
                    url,
                }
            }
        })
        .collect();

    links
}

fn extract_tags(todo_text: &str) -> Vec<Tag> {
    lazy_static! {
        static ref TAG: Regex = Regex::new(r"(?P<tag>#[^\#\s]+)").unwrap();
    }

    let tags = TAG
        .captures_iter(todo_text)
        .flat_map(|capture| {
            option! {
                let text_and_start_offset = capture
                    .name("tag")
                    .map(|t| (t.as_str().to_string(), t.start()));

                let match_ <- capture.get(0);
                let start_offset = match_.start();
                let length = match_.end() - start_offset;

                let (mut text, start_offset) <- text_and_start_offset;
                text = text[1..].to_string();

                Tag {
                    start_offset,
                    length,
                    text,
                }
            }
        })
        .collect();

    tags
}

fn extract_info(todo_line: &str) -> Result<TodoInfo, MxtodoSearcherError> {
    lazy_static! {
        static ref TODO: Regex = Regex::new(r"^- \[(?P<completed>[Xx]|\s)\]\s+(?P<text>.*?)(?: *\(due (?P<date_due>[^)]+)\))?(?: *\(completed (?P<date_completed>[^)]+)\))?$").unwrap();
    }

    TODO.captures(todo_line)
        .map(|captures| {
            let is_completed = captures
                .name("completed")
                .map(|s| s.as_str().to_lowercase() == "x")
                .unwrap_or_default();

            let text = captures
                .name("text")
                .map(|m| m.as_str().to_string())
                .ok_or(MxtodoSearcherError::TodoParsingError(todo_line.to_string()))?;

            let date_due = captures
                .name("date_due")
                .map(|m| utc_datetime_from_date_str_with_optional_time(m.as_str()))
                .transpose()?;

            let date_completed = captures
                .name("date_completed")
                .map(|m| utc_datetime_from_date_str_with_optional_time(m.as_str()))
                .transpose()?;

            let links = extract_links(&text);
            let tags = extract_tags(&text);

            Ok(TodoInfo {
                is_completed,
                text,
                date_due,
                date_completed,
                links,
                tags,
            })
        })
        .ok_or(MxtodoSearcherError::TodoParsingError(todo_line.to_string()))
        .flatten()
}

impl Todo {
    pub fn from_match(todo_match: &TodoMatch) -> Result<Todo, MxtodoSearcherError> {
        let file_path = todo_match.file_path.to_owned();
        let line_number = todo_match.line_number;
        let line = todo_match.line.to_owned();
        let display_date: DateTime<Utc> = display_date_from_file_path(&file_path)?;
        let TodoInfo {
            is_completed,
            text,
            date_due,
            date_completed,
            links,
            tags,
        } = extract_info(&line)?;

        Ok(Todo {
            file_path,
            line_number,
            display_date,
            date_due,
            date_completed,
            text,
            is_completed,
            links,
            tags,
        })
    }
}

type TodosAndErrors = (Vec<Todo>, Vec<MxtodoSearcherError>);

pub fn _search_directory(
    directory: &str,
    file_ext: &str,
    pattern: &str,
) -> Result<TodosAndErrors, MxtodoSearcherError> {
    if !Path::new(directory).exists() {
        return Err(MxtodoSearcherError::DirectoryNotFound(
            directory.to_string(),
        ));
    }

    let md = Path::new(directory).metadata().map_err(|_| {
        MxtodoSearcherError::UnexpectedIOError(format!(
            "Error reading path metadata on {:?}",
            directory
        ))
    })?;

    if !md.is_dir() {
        return Err(MxtodoSearcherError::InvalidPathParameter(
            directory.to_string(),
        ));
    }

    let matcher = RegexMatcherBuilder::new()
        .multi_line(true)
        .line_terminator(Some(b'\n'))
        .build(&pattern)
        .map_err(|_e| MxtodoSearcherError::MalformedRegexPattern(pattern.to_string()))?;

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

            let _ = searcher
                .search_path(&matcher, &file_path, results_sink)
                .map_err(|e| MxtodoSearcherError::SearchError(e.to_string()));

            Ok(results)
        })
        .collect::<Result<Vec<Vec<TodoMatch>>, MxtodoSearcherError>>()?;

    let (todos, errors): (Vec<_>, Vec<_>) = found
        .iter()
        .flat_map(|matches| matches.into_iter().map(|m| Todo::from_match(m)))
        .partition_map(|todo_or_error| match todo_or_error {
            Ok(todo) => Either::Left(todo),
            Err(e) => Either::Right(e),
        });

    Ok((todos, errors))
}

emacs::define_errors! {
    directory_not_found "The specified directory does not exist" (file_error)
    path_is_not_a_directory "The specified path is not a directory" (file_error)
    searcher_error "An expected error occurred"
}

/// This searches `directory` for files with extension `file_ext` returning a list of todos as 2-element vectors.
#[defun]
fn search_directory(
    env: &Env,
    directory: String,
    file_ext: String,
    pattern: String,
) -> emacs::Result<Value> {
    let result = _search_directory(&directory, &file_ext, &pattern);
    if result.is_err() {
        match result {
            Err(MxtodoSearcherError::DirectoryNotFound(_)) => {
                return env.signal(directory_not_found, (directory,))
            }
            Err(MxtodoSearcherError::InvalidPathParameter(_)) => {
                return env.signal(path_is_not_a_directory, (directory,))
            }
            _ => return env.signal(searcher_error, (directory, file_ext, pattern)),
        }
    }

    let (todos, errors) = result.unwrap();

    if errors.len() > 0 {
        env.message(format!(
            "Encountered {} errors while searching",
            errors.len()
        ))?;
    }

    let todos_lisp: emacs::Result<Vec<Value>> =
        todos.into_iter().map(|todo| todo.into_lisp(env)).collect();

    env.vector(&todos_lisp?)
}

#[cfg(test)]
mod tests {
    use crate::*;
    use std::fs::File;
    use std::io::Write;
    use std::path::PathBuf;
    use tempfile;

    #[test]
    fn files_to_search_returns_the_expected_files() -> Result<(), String> {
        let dir = tempfile::tempdir()
            .map_err(|e| format!("Unable to create a temporary directory: {:?}", e))?;
        let file_path = dir.path().join("my-temporary-file.md");
        let mut file1 = File::create(&file_path)
            .map_err(|e| format!("Unable to create a temp file: {:?}", e))?;
        writeln!(file1, "Test data.")
            .map_err(|e| format!("Unable to write to temp file: {:?}", e))?;

        let expected = vec![file_path.into_os_string()];
        let actual = files_to_search(&dir.into_path().into_os_string().to_string_lossy(), ".md");
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn files_to_search_returns_no_files() -> Result<(), String> {
        let dir = tempfile::tempdir()
            .map_err(|e| format!("Unable to create a temporary directory: {:?}", e))?;
        let file_path = dir.path().join("my-temporary-file.derp");
        let mut file1 = File::create(&file_path)
            .map_err(|e| format!("Unable to create a temp file: {:?}", e))?;
        writeln!(file1, "Test data.")
            .map_err(|e| format!("Unable to write to temp file: {:?}", e))?;

        let expected: Vec<OsString> = vec![];
        let actual = files_to_search(&dir.into_path().into_os_string().to_string_lossy(), ".md");
        assert_eq!(expected, actual);

        Ok(())
    }

    fn create_test_file(filename: &str) -> Result<(tempfile::TempDir, PathBuf), String> {
        let dir = tempfile::tempdir()
            .map_err(|e| format!("Unable to create a temporary directory: {:?}", e))?;
        let file_path: PathBuf = dir.path().join(filename);
        let mut file1 = File::create(&file_path)
            .map_err(|e| format!("Unable to create a temp file: {:?}", e))?;
        writeln!(file1, "- [ ] do your taxes")
            .map_err(|e| format!("Unable to write to temp file: {:?}", e))?;

        Ok((dir, file_path))
    }

    #[test]
    fn search_directory_returns_the_expected_matches() -> Result<(), String> {
        let (dir, file_path) = create_test_file("2022-2-11.md")?;
        let pattern = r"^- ?\[[Xx ]\] ".to_string();

        let expected: TodosAndErrors = (
            vec![Todo::new(
                file_path
                    .clone()
                    .into_os_string()
                    .to_string_lossy()
                    .to_string(),
                1,
                Utc.with_ymd_and_hms(2022, 2, 11, 0, 0, 0).unwrap(),
                None,
                None,
                "do your taxes".to_string(),
                false,
                vec![],
                vec![],
            )],
            vec![],
        );

        let (expected_todos, _) = expected;

        let (actual_todos, actual_errors) = _search_directory(
            &dir.into_path().into_os_string().to_string_lossy(),
            ".md",
            &pattern,
        )
        .unwrap();

        assert_eq!(expected_todos, actual_todos);
        assert!(actual_errors.is_empty());

        Ok(())
    }

    #[test]
    fn extract_info_returns_the_correct_due_date_in_utc() -> Result<(), String> {
        let text = "ride a boat".to_string();
        let is_completed = false;
        let date_due = Some(Utc.with_ymd_and_hms(2022, 8, 6, 0, 0, 0).unwrap());
        let date_completed = None;
        let links: Vec<Link> = vec![];
        let tags: Vec<Tag> = vec![];
        let todo_line = format!("- [ ] {text} (due 2022-08-06)", text = text);

        let expected: TodoInfo = TodoInfo {
            is_completed,
            text,
            date_due,
            date_completed,
            links,
            tags,
        };

        let actual = extract_info(&todo_line).unwrap();

        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn extract_links_returns_no_links_when_there_are_none() -> Result<(), String> {
        let text = "- [ ] read a book";
        let expected: Vec<Link> = vec![];
        let actual = extract_links(text);
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn extract_links_returns_the_expected_link_when_there_is_one() -> Result<(), String> {
        let text = "- [ ] read [the news](www.thenews.com)";
        let expected = vec![Link {
            start_offset: 11,
            text_start_offset: 12,
            url_start_offset: 22,
            length: 27,
            text: "the news".to_string(),
            url: "www.thenews.com".to_string(),
        }];

        let actual = extract_links(text);
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn extract_links_returns_all_the_expected_links() -> Result<(), String> {
        let text = "- [ ] read [the news](www.thenews.com) and [a book](www.abook.com)";
        let expected = vec![
            Link {
                start_offset: 11,
                text_start_offset: 12,
                url_start_offset: 22,
                length: 27,
                text: "the news".to_string(),
                url: "www.thenews.com".to_string(),
            },
            Link {
                start_offset: 43,
                text_start_offset: 44,
                url_start_offset: 52,
                length: 23,
                text: "a book".to_string(),
                url: "www.abook.com".to_string(),
            },
        ];

        let actual = extract_links(text);
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn extract_tags_returns_no_tags_when_there_are_none() -> Result<(), String> {
        let text = "- [ ] read a book";
        let expected: Vec<Tag> = vec![];
        let actual = extract_tags(text);
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn extract_tags_returns_the_expected_tag_when_there_is_one() -> Result<(), String> {
        let text = "- [ ] go for a jog #exercise";
        let expected = vec![Tag {
            start_offset: 19,
            length: 9,
            text: "exercise".to_string(),
        }];
        let actual = extract_tags(text);
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn extract_tags_returns_all_the_expected_tags() -> Result<(), String> {
        let text = "- [ ] take kids to school #parenting #home";
        let expected = vec![
            Tag {
                start_offset: 26,
                length: 10,
                text: "parenting".to_string(),
            },
            Tag {
                start_offset: 37,
                length: 5,
                text: "home".to_string(),
            },
        ];
        let actual = extract_tags(text);
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn extract_info_returns_the_correct_completed_date_in_utc() -> Result<(), String> {
        let text = "add some details for Jira ticket".to_string();
        let is_completed = true;
        let date_due = None;
        let date_completed = Some(Utc.with_ymd_and_hms(2023, 6, 28, 10, 19, 13).unwrap());
        let links: Vec<Link> = vec![];
        let tags: Vec<Tag> = vec![];
        let todo_line = format!("- [x] {text} (completed 2023-06-28T10:19:13Z)", text = text);

        let expected: TodoInfo = TodoInfo {
            is_completed,
            text,
            date_due,
            date_completed,
            links,
            tags,
        };

        let actual = extract_info(&todo_line).unwrap();
        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn udfdswot_given_well_formed_date_str_returns_a_utc_datetime() -> Result<(), String> {
        let expected = Utc.with_ymd_and_hms(2022, 9, 25, 0, 0, 0).unwrap();
        let actual = utc_datetime_from_date_str_with_optional_time("2022-09-25").unwrap();

        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn udfdswot_given_well_formed_datetime_str_returns_a_utc_datetime() -> Result<(), String> {
        let expected = Utc.with_ymd_and_hms(2022, 9, 25, 0, 0, 0).unwrap();
        let actual = utc_datetime_from_date_str_with_optional_time("2022-09-25T00:00:00Z").unwrap();

        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn udfdswot_given_malformed_date_str_returns_an_error() -> Result<(), String> {
        let expected = MxtodoSearcherError::MalformedDateString("foo bar baz today".to_string());
        let actual =
            utc_datetime_from_date_str_with_optional_time("foo bar baz today").unwrap_err();

        assert_eq!(expected, actual);

        Ok(())
    }
}
