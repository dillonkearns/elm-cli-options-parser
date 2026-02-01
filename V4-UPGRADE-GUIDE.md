# Upgrading to elm-cli-options-parser v4

This guide covers the breaking changes in v4 and how to migrate your code.

## Summary of Changes

v4 brings three main improvements:

1. **Color support** - Help text and error messages now support ANSI colors
2. **Better error messages** - More specific errors with custom message support
3. **Testing support** - New `Program.run` function for unit testing your CLI

## Breaking Changes

### 1. Add `colorMode` to your JavaScript flags

Your JavaScript initialization **must** now include a `colorMode` boolean.

**Before (v3):**
```javascript
Elm.Main.init({
  flags: {
    argv: process.argv,
    versionMessage: "1.0.0"
  }
});
```

**After (v4):**
```javascript
// Detect color support: enabled for TTY unless NO_COLOR is set
const useColor = !!(process.stdout.isTTY && !process.env.NO_COLOR);

Elm.Main.init({
  flags: {
    argv: process.argv,
    versionMessage: "1.0.0",
    colorMode: useColor
  }
});
```

When `colorMode` is `true`, output includes ANSI escape codes:
```
Missing required option: --name   ← displayed in red
```

When `colorMode` is `false`, output is plain text:
```
Missing required option: --name
```

### 2. Rename `withDoc` to `withDescription`

The function was renamed for consistency with the new `Option.withDescription`.

**Before:**
```elm
OptionsParser.buildSubCommand "init" Init
    |> OptionsParser.withDoc "initialize a git repository"
```

**After:**
```elm
OptionsParser.buildSubCommand "init" Init
    |> OptionsParser.withDescription "initialize a git repository"
```

### 3. `oneOf` no longer takes a default value argument

The first argument (which was ignored) has been removed.

**Before:**
```elm
Option.optionalKeywordArg "format"
    |> Option.withDefault "json"
    |> Option.oneOf Json   -- First arg was ignored
        [ ( "json", Json )
        , ( "xml", Xml )
        ]
```

**After:**
```elm
Option.optionalKeywordArg "format"
    |> Option.withDefault "json"
    |> Option.oneOf
        [ ( "json", Json )
        , ( "xml", Xml )
        ]
```

## New Features

### Option Descriptions

Add descriptions to individual options. These appear in `--help` output.

```elm
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build GreetOptions
                |> OptionsParser.with
                    (Option.requiredKeywordArg "name"
                        |> Option.withDescription "Your name for the greeting"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "greeting"
                        |> Option.withDescription "Custom greeting (default: Hello)"
                    )
            )
```

**Help output:**
```
$ ./greet --help
Usage: greet --name <name> [--greeting <greeting>]

Options:
  --name <name>           Your name for the greeting
  --greeting <greeting>   Custom greeting (default: Hello)
```

### Custom Missing Argument Messages

For required options, provide a custom error message when the argument is missing.

```elm
Option.requiredPositionalArg "repository"
    |> Option.withMissingMessage "You must specify a repository to clone."
```

**Default error:**
```
Missing required argument: <repository>

myprog clone <repository>
```

**With custom message:**
```
You must specify a repository to clone.

myprog clone <repository>
```

This is useful for providing actionable guidance:

```elm
Option.requiredKeywordArg "token"
    |> Option.withMissingMessage
        "Authentication required. Get a token from https://example.com/settings"
```

### Testing with `Program.run`

Test your CLI configuration without the full Platform.Program infrastructure.

```elm
import Cli.Program as Program
import Expect
import Test exposing (..)

suite : Test
suite =
    describe "My CLI"
        [ test "parses --name correctly" <|
            \() ->
                case Program.run myConfig [ "node", "myprog", "--name", "Alice" ] "1.0.0" Program.WithoutColor of
                    Program.CustomMatch options ->
                        options.name |> Expect.equal "Alice"

                    Program.SystemMessage _ message ->
                        Expect.fail ("Unexpected: " ++ message)

        , test "shows error for missing --name" <|
            \() ->
                case Program.run myConfig [ "node", "myprog" ] "1.0.0" Program.WithoutColor of
                    Program.SystemMessage Program.Failure message ->
                        message |> String.contains "Missing required option: --name"
                            |> Expect.true "Should mention missing --name"

                    _ ->
                        Expect.fail "Expected error"
        ]
```

**Tip:** Use `Program.WithoutColor` in tests so assertions work on plain text.

### Subcommand-specific Help

Users can now get help for a specific subcommand:

```
$ ./git clone --help
Usage: git clone <repository>

$ ./git log --help
Usage: git log [--author <author>] [--oneline]
```

## Migration Checklist

- [ ] Add `colorMode: true/false` to your JavaScript flags
- [ ] Rename `OptionsParser.withDoc` to `OptionsParser.withDescription`
- [ ] Remove the first argument from any `Option.oneOf` calls
- [ ] (Optional) Add `Option.withDescription` to improve your `--help` output
- [ ] (Optional) Add `Option.withMissingMessage` for better error UX
- [ ] (Optional) Add tests using `Program.run`
