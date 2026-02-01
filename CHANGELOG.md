# Changelog

All notable changes to
[the `dillonkearns/elm-cli-options-parser` elm package](https://github.com/dillonkearns/elm-cli-options-parser)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [4.0.0]

See the [V4 Upgrade Guide](V4-UPGRADE-GUIDE.md) for migration instructions.

### Added

- **Color support**: Help text and error messages now support ANSI colors. Pass `colorMode: true/false` in your JavaScript flags to enable/disable.
- **`Option.withDescription`**: Add descriptions to individual options that appear in `--help` output.
- **`Option.withMissingMessage`**: Provide custom error messages for required options when they're missing.
- **`Program.run`**: Test your CLI configuration without the full Platform.Program infrastructure. Returns a `RunResult` that you can pattern match on in tests.
- **`Program.ColorMode`**, **`Program.ExitStatus`**, **`Program.RunResult`**: New types for the testing API.
- **Subcommand-specific help**: Users can now run `myprogram subcommand --help` to get help for a specific subcommand.

### Changed

- **Breaking**: JavaScript flags must now include `colorMode: Bool` field.
- **Breaking**: `OptionsParser.withDoc` renamed to `OptionsParser.withDescription` for consistency with `Option.withDescription`.
- **Breaking**: `Option.oneOf` no longer takes a default value as its first argument (it was ignored anyway).
- `Option` type is now a type alias to an internal type. This shouldn't require changes to your code.
- `subscriptions` in `StatefulOptions` now receives `cliOptions` as first argument, allowing access to parsed CLI options from subscriptions.

### Improved

- Better error messages with color highlighting for option names and error text.
- Typo suggestions now highlight the suggested option name.
- Documentation improvements throughout.

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
