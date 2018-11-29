# Elm CLI Options Parser

[![Build Status](https://travis-ci.org/dillonkearns/elm-cli-options-parser.svg?branch=master)](https://travis-ci.org/dillonkearns/elm-cli-options-parser)

`elm-cli-options-parser` allows you to build command-line options parsers in Elm.
It uses a syntax similar to `Json.Decode.Pipeline`.

You can
play around with `elm-cli-options-parser` in a [live terminal simulation in Ellie here](https://rebrand.ly/elm-cli)!

## Example

See the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder for full end-to-end examples, including how to wire
your Elm options parser up through NodeJS so it can receive the command line input.

Take this `git` command:

```console
git log --author=dillon --max-count=5 --stat a410067
```

To parse the above command, we could build a `Program` as follows (this snippet doesn't include the wiring of the OptionsParser-Line options from NodeJS, see the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder):

```elm
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (with)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.Program as Program


type CliOptions
    = Init
    | Clone String
    | Log LogOptions


type alias LogOptions =
    { maybeAuthorPattern : Maybe String
    , maybeMaxCount : Maybe Int
    , statisticsMode : Bool
    , maybeRevisionRange : Maybe String
    , restArgs : List String
    }

programConfig : Program.Config CliOptions
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.buildSubCommand "init" Init
                |> OptionsParser.withDoc "initialize a git repository"
            )
        |> Program.add
            (OptionsParser.buildSubCommand "clone" Clone
                |> with (Option.requiredPositionalArg "repository")
            )
        |> Program.add (OptionsParser.map Log logOptionsParser)


logOptionsParser : OptionsParser.OptionsParser LogOptions BuilderState.NoMoreOptions
logOptionsParser =
    OptionsParser.buildSubCommand "log" LogOptions
        |> with (Option.optionalKeywordArg "author")
        |> with
            (Option.optionalKeywordArg "max-count"
                |> Option.validateMapIfPresent String.toInt
            )
        |> with (Option.flag "stat")
        |> OptionsParser.withOptionalPositionalArg
            (Option.optionalPositionalArg "revision range")
        |> OptionsParser.withRestArgs
            (Option.restArgs "rest args")
```

```elm
{-
Now running:
`git log --author=dillon --max-count=5 --stat a410067`
will yield the following output (with wiring as in the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder):
-}
matchResult : CliOptions
matchResult =
    Log
        { maybeAuthorPattern = Just "dillon"
        , maybeMaxCount = Just 5
        , statisticsMode = True
        , revisionRange = Just "a410067"
        }
```

It will also generate the help text for you, so it's guaranteed to be in sync.
The example code above will generate the following help text:

```console
$ ./git --help
git log [--author <author>] [--max-count <max-count>] [--stat] [<revision range>]
```

Note: the `--help` option is a built-in command, so no need to write a `OptionsParser` for that.

## Design Goals

1. **Build in great UX by design**
   For example, single character options like `-v` can be confusing.
   Are they always confusing? Maybe not, but eliminating the possibility makes
   things much more explicit and predictable.
   For example, `grep -v` is an alias for `--invert-match` (`-V` is the alias
   for `--version`). And there is a confusing and somewhat ambiguous syntax for
   passing arguments to single character flags
   (for example, you can group multiple flags like `grep -veabc`, which is the
   same as `grep --invert-match --regexp=abc`). This is difficult for humans to
   parse or remember, and this library is opinionated about doing things in a
   way that is very explicit, unambiguous, and easy to understand.

   Another example, the `--help` flag should always be there and work in a standard way...
   so this is baked into the library rather than being an optional or a manual
   configuration.

1. **Guaranteed to be in-sync** - by automatically generating help messages
   you know that users are getting the right information. The design of the
   validation API also ensures that users get focused errors that point to
   exactly the point of failure and the reason for the failure.

1. **Be explicit and unambiguous** - like the Elm ethos, this library aims to give you very clear error
   messages the instant it knows the options can't be parsed, rather than when it
   discovers it's missing something it requires. For example, if you
   pass in an unrecognized flag, you will immediately get an error with typo
   suggestions.
   Another example, this library [enforces that you don't specify an ambiguous mix of optional
   and required positional args](#todo-link). This could easily be fixed with
   some convention to move all optional arguments to the very end regardless of
   what order you specify them in, but this would go against this value of
   explicitness.

## Options Parser Terminology

Here is a diagram to clarify the terminology used by this library. Note that
terms can vary across different standards. For example, posix uses the term
`option` for what this library calls a `keyword argument`. I chose these terms
because I found them to be the most intuitive and unambiguous.

<img src="https://raw.githubusercontent.com/dillonkearns/elm-cli-options-parser/master/terminology.png" alt="Terminology Legend" width="600px"/>

## Some Inspiration for this package

- http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html.
- https://pythonconquerstheuniverse.wordpress.com/2010/07/25/command-line-syntax-some-basic-concepts/
- https://devcenter.heroku.com/articles/cli-style-guide
- http://docopt.org/
