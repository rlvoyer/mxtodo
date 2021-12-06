# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.1] - 2021-12-05

## [0.2.0] - 2021-10-11
- replaced use of ripgrep and jq via shell command from elisp with ripgrep-backed native module
- reverted to using `ts` timestamps for all time fields

## [0.1.3] - 2021-08-20
- fixed bug affecting todo creation
- added a release script

## [0.1.2] - 2021-08-18
- added logic to ensure that note file has not been modified since last write before updating

## [0.1.1] - 2021-08-07
- improved handling of due date parsing errors in `mxtodo-create-todo`

## [0.1.0] - 2021-08-07
- added this CHANGELOG file
- added initial README
- added basic functionality for listing and managing Markdown TODOs
