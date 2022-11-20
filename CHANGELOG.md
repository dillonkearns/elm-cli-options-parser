# Changelog

All notable changes to
[the `dillonkearns/elm-cli-options-parser` elm package](https://github.com/dillonkearns/elm-cli-options-parser)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.2.0] - 2022-10-19

### Added

- Added `Program.mapConfig` helper.

## [3.0.1] - 2020-3-15

### Changed

- Update ellie example link.

## [3.0.0] - 2018-11-29

### Fixed

- Rename `version` flag to `versionMessage` to avoid a compiler bug.

## [2.0.0] - 2018-11-29

### Changes

- Add `version` as a new required Flag, and removed `version` from the `config`
  constructor function. The String that is passed in as `version` from NodeJS
  will be printed out verbatim when you call `your-cli --version`.
