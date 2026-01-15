module Cli.Program exposing
    ( config, Config, add
    , stateless, ProgramOptions, stateful, StatefulOptions
    , StatelessProgram, StatefulProgram
    , FlagsIncludingArgv
    , mapConfig
    , run, RunResult(..)
    )

{-|


## Building a Config

A `Cli.Program.Config` is created with `Cli.Program.config`. Then `OptionsParser`s are added
to it with `Cli.Program.add`. Finally, you create a `Cli.Program.StatelessProgram`
using `stateless` or a `Cli.Program.StatefulProgram` using `stateful`.

    import Cli.Option as Option
    import Cli.OptionsParser as OptionsParser
    import Cli.Program as Program
    import Ports

    programConfig : Program.Config GreetOptions
    programConfig =
        Program.config
            |> Program.add
                (OptionsParser.build GreetOptions
                    |> OptionsParser.with (Option.requiredKeywordArg "name")
                    |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                )

    type alias GreetOptions =
        { name : String
        , maybeGreeting : Maybe String
        }

    init : Flags -> GreetOptions -> Cmd Never
    init flags { name, maybeGreeting } =
        maybeGreeting
            |> Maybe.withDefault "Hello"
            |> (\greeting -> greeting ++ " " ++ name ++ "!")
            |> Ports.print

    type alias Flags =
        Program.FlagsIncludingArgv {}

    main : Program.StatelessProgram Never
    main =
        Program.stateless
            { printAndExitFailure = Ports.printAndExitFailure
            , printAndExitSuccess = Ports.printAndExitSuccess
            , init = init
            , config = programConfig
            }

See the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) for some end-to-end examples.

@docs config, Config, add


## `Program`s

@docs stateless, ProgramOptions, stateful, StatefulOptions
@docs StatelessProgram, StatefulProgram
@docs FlagsIncludingArgv
@docs mapConfig


## Low-Level / Testing

@docs run, RunResult

-}

import Cli.ExitStatus exposing (ExitStatus)
import Cli.LowLevel
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.OptionsParser.MatchResult exposing (NoMatchReason(..))
import List.Extra
import TypoSuggestion


{-| The result of running the CLI parser. Useful for testing.

  - `SystemMessage exitStatus message` - A system message (help, version, or error) with exit status
  - `CustomMatch match` - Successfully matched and parsed the CLI options

-}
type RunResult match
    = SystemMessage ExitStatus String
    | CustomMatch match


{-| A `Cli.Program.Config` is used to build up a set of `OptionsParser`s for your
Command-Line Interface, as well as its meta-data such as version number.
-}
type Config msg
    = Config
        { optionsParsers : List (OptionsParser msg BuilderState.NoMoreOptions)
        }


{-| Create a `Config` with no `OptionsParser`s. Use `Cli.Program.add` to add
`OptionsParser`s.
-}
config : Config decodesTo
config =
    Config
        { optionsParsers = []
        }


{-| Add an `OptionsParser` to your `Cli.Program.Config`.
-}
add : OptionsParser msg anything -> Config msg -> Config msg
add optionsParser (Config ({ optionsParsers } as programRecord)) =
    Config
        { programRecord
            | optionsParsers = optionsParsers ++ [ OptionsParser.end optionsParser ]
        }


{-| Flags in Cli Programs can contain any data as long as it is a record
at the top-level which contains an `argv` field of type `List String`.
In other words, it must be a record of type `FlagsIncludingArgv`
(if you aren't familiar with them, you can [read more about extensible records here](https://medium.com/@ckoster22/advanced-types-in-elm-extensible-records-67e9d804030d)).

You pass in the flags like this (see the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder for more):

```javascript
#!/usr/bin/env node

let program = require("./elm.js").Elm.Main.init({
  flags: { argv: process.argv, versionMessage: "1.2.3" }
});
```

-}
type alias FlagsIncludingArgv flagsRecord =
    { flagsRecord
        | argv : List String
        , versionMessage : String
    }


{-| A program that processes arguments and exits. Use with `stateless`.
-}
type alias StatelessProgram msg flags =
    Platform.Program (FlagsIncludingArgv flags) () msg


{-| Create a CLI that processes arguments and exits immediately.
Use `stateful` instead if you need to perform `Cmd`s (HTTP, etc.).
-}
stateless : ProgramOptions msg options flags -> StatelessProgram msg flags
stateless options =
    Platform.worker
        { init = init options
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


{-| A program with a model and update loop. Use with `stateful`.
-}
type alias StatefulProgram model msg cliOptions flags =
    Platform.Program (FlagsIncludingArgv flags) (StatefulProgramModel model cliOptions) msg


{-| Configuration for a stateful CLI program. Pass this record to [`stateful`](#stateful).

Stateful programs work like standard Elm programs with a model, update loop, and
subscriptions. Use this when your CLI needs to wait for responses (e.g., HTTP requests)
or maintain state across multiple events. The parsed CLI options are passed to both
`init` and `update`.

  - `printAndExitFailure` - Port to print a message and exit with a non-zero status code
  - `printAndExitSuccess` - Port to print a message and exit with status code 0
  - `init` - Initialize your model with the parsed CLI options
  - `update` - Handle messages and update your model (also receives CLI options)
  - `subscriptions` - Subscribe to external events
  - `config` - The CLI configuration built with [`config`](#config) and [`add`](#add)

-}
type alias StatefulOptions msg model cliOptions flags =
    { printAndExitFailure : String -> Cmd msg
    , printAndExitSuccess : String -> Cmd msg
    , init : FlagsIncludingArgv flags -> cliOptions -> ( model, Cmd msg )
    , update : cliOptions -> msg -> model -> ( model, Cmd msg )
    , subscriptions : cliOptions -> model -> Sub msg
    , config : Config cliOptions
    }


{-| A `stateful` program can have a model that it creates and updates via `init`
and `update`. It also has `subscriptions`. See
[the `Curl.elm` example](https://github.com/dillonkearns/elm-cli-options-parser/blob/master/examples/src/Curl.elm).
-}
stateful :
    StatefulOptions msg model cliOptions flags
    -> Platform.Program (FlagsIncludingArgv flags) (StatefulProgramModel model cliOptions) msg
stateful options =
    Platform.worker
        { init = statefulInit options
        , update =
            \msg model ->
                case model of
                    UserModel actualModel cliOptions ->
                        let
                            ( userModel, userCmd ) =
                                options.update cliOptions msg actualModel
                        in
                        ( UserModel userModel cliOptions, userCmd )

                    ShowSystemMessage ->
                        ( ShowSystemMessage, Cmd.none )
        , subscriptions =
            \model ->
                case model of
                    UserModel actualModel cliOptions ->
                        options.subscriptions cliOptions actualModel

                    ShowSystemMessage ->
                        Sub.none
        }


{-| Configuration for a stateless CLI program. Pass this record to [`stateless`](#stateless).

Stateless programs run once and exit - there is no persistent model or update loop.
Your `init` receives the parsed CLI options and returns a `Cmd` that performs the
program's work, then the program is done.

  - `printAndExitFailure` - Port to print a message and exit with a non-zero status code
  - `printAndExitSuccess` - Port to print a message and exit with status code 0
  - `init` - Receives parsed CLI options and returns a `Cmd` to perform the program's work
  - `config` - The CLI configuration built with [`config`](#config) and [`add`](#add)

-}
type alias ProgramOptions decodesTo options flags =
    { printAndExitFailure : String -> Cmd decodesTo
    , printAndExitSuccess : String -> Cmd decodesTo
    , init : FlagsIncludingArgv flags -> options -> Cmd decodesTo
    , config : Config options
    }


init :
    ProgramOptions msg options flags
    -> FlagsIncludingArgv flags
    -> ( (), Cmd msg )
init options flags =
    let
        matchResult : RunResult options
        matchResult =
            run options.config flags.argv flags.versionMessage

        cmd =
            case matchResult of
                SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.ExitStatus.Failure ->
                            options.printAndExitFailure message

                        Cli.ExitStatus.Success ->
                            options.printAndExitSuccess message

                CustomMatch msg ->
                    options.init flags msg
    in
    ( (), cmd )


type StatefulProgramModel model cliOptions
    = ShowSystemMessage
    | UserModel model cliOptions


statefulInit :
    StatefulOptions msg model cliOptions flags
    -> FlagsIncludingArgv flags
    -> ( StatefulProgramModel model cliOptions, Cmd msg )
statefulInit options flags =
    let
        matchResult : RunResult cliOptions
        matchResult =
            run options.config flags.argv flags.versionMessage
    in
    case matchResult of
        SystemMessage exitStatus message ->
            case exitStatus of
                Cli.ExitStatus.Failure ->
                    ( ShowSystemMessage, options.printAndExitFailure message )

                Cli.ExitStatus.Success ->
                    ( ShowSystemMessage, options.printAndExitSuccess message )

        CustomMatch cliOptions ->
            let
                ( userModel, userCmd ) =
                    options.init flags cliOptions
            in
            ( UserModel userModel cliOptions, userCmd )


{-| Run the CLI parser directly and get back a `RunResult`. This is useful for testing
your CLI configuration without needing to set up the full Platform.Program infrastructure.

    import Cli.ExitStatus
    import Cli.Program as Program

    -- Test that missing required arg shows error
    case Program.run myConfig [ "node", "myprog" ] "1.0.0" of
        Program.SystemMessage Cli.ExitStatus.Failure message ->
            -- Assert on the error message
            String.contains "Missing" message

        _ ->
            False

Note: `argv` should include the node path and script path as the first two elements,
just like `process.argv` in Node.js.

-}
run : Config msg -> List String -> String -> RunResult msg
run (Config { optionsParsers }) argv versionMessage =
    let
        programName =
            case argv of
                _ :: programPath :: _ ->
                    programPath
                        |> String.split "/"
                        |> List.Extra.last
                        |> Maybe.withDefault errorMessage

                _ ->
                    errorMessage

        errorMessage =
            "TODO - show error message explaining that user needs to pass unmodified `process.argv` from node here."

        matchResult =
            Cli.LowLevel.try optionsParsers argv

        parserInfo =
            optionsParsers
                |> List.map
                    (\optionsParser ->
                        { usageSpecs = OptionsParser.getUsageSpecs optionsParser
                        , subCommand = OptionsParser.getSubCommand optionsParser
                        }
                    )

        availableSubCommands =
            optionsParsers
                |> List.filterMap OptionsParser.getSubCommand
    in
    case matchResult of
        Cli.LowLevel.NoMatch reasons ->
            formatNoMatchReasons programName parserInfo availableSubCommands optionsParsers reasons
                |> SystemMessage Cli.ExitStatus.Failure

        Cli.LowLevel.ValidationErrors validationErrors ->
            ("Validation errors:\n\n"
                ++ (validationErrors
                        |> List.map
                            (\{ name, invalidReason } ->
                                "Invalid "
                                    ++ "`--"
                                    ++ name
                                    ++ "` option."
                                    ++ "\n"
                                    ++ invalidReason
                            )
                        |> String.join "\n"
                   )
            )
                |> SystemMessage Cli.ExitStatus.Failure

        Cli.LowLevel.Match msg ->
            msg
                |> CustomMatch

        Cli.LowLevel.ShowHelp ->
            Cli.LowLevel.detailedHelpText programName optionsParsers
                |> SystemMessage Cli.ExitStatus.Success

        Cli.LowLevel.ShowVersion ->
            versionMessage
                |> SystemMessage Cli.ExitStatus.Success

        Cli.LowLevel.ShowSubcommandHelp subcommandName ->
            subcommandHelpText programName optionsParsers subcommandName
                |> SystemMessage Cli.ExitStatus.Success


{-| Transform the return type for all of the registered `OptionsParser`'s in the `Config`.
-}
mapConfig : (a -> b) -> Config a -> Config b
mapConfig mapFn (Config configValue) =
    Config
        { optionsParsers =
            configValue.optionsParsers
                |> List.map (OptionsParser.map mapFn)
        }


{-| Generate help text for a specific subcommand.
-}
subcommandHelpText : String -> List (OptionsParser msg BuilderState.NoMoreOptions) -> String -> String
subcommandHelpText programName optionsParsers subcommandName =
    optionsParsers
        |> List.filter (\parser -> OptionsParser.getSubCommand parser == Just subcommandName)
        |> List.map (OptionsParser.detailedHelp programName)
        |> String.join "\n\n"


{-| Format NoMatchReasons into a user-friendly error message.
-}
formatNoMatchReasons :
    String
    -> List TypoSuggestion.OptionsParser
    -> List String
    -> List (OptionsParser msg BuilderState.NoMoreOptions)
    -> List NoMatchReason
    -> String
formatNoMatchReasons programName parserInfo availableSubCommands optionsParsers reasons =
    let
        -- Separate unexpected options from other reasons
        unexpectedOptions =
            reasons
                |> List.filterMap
                    (\reason ->
                        case reason of
                            UnexpectedOption name ->
                                Just name

                            _ ->
                                Nothing
                    )

        otherReasons =
            reasons
                |> List.filter
                    (\reason ->
                        case reason of
                            UnexpectedOption _ ->
                                False

                            _ ->
                                True
                    )

        -- Check for subcommand-related errors
        hasSubCommandParsers =
            not (List.isEmpty availableSubCommands)

        missingSubCommandReasons =
            otherReasons
                |> List.filterMap
                    (\reason ->
                        case reason of
                            MissingSubCommand _ ->
                                Just reason

                            _ ->
                                Nothing
                    )

        wrongSubCommandReasons =
            otherReasons
                |> List.filterMap
                    (\reason ->
                        case reason of
                            WrongSubCommand { actualSubCommand } ->
                                Just actualSubCommand

                            _ ->
                                Nothing
                    )

        -- "Specific" errors indicate a parser got further in matching
        -- (matched subcommand or structure, but failed on argument details)
        -- These should take priority over "subcommand not found" errors
        -- Note: ExtraOperand is NOT included here because it's often from
        -- system parsers (help/version) and isn't specific enough
        missingArgErrors =
            otherReasons
                |> List.filter
                    (\reason ->
                        case reason of
                            MissingRequiredPositionalArg _ ->
                                True

                            MissingRequiredKeywordArg _ ->
                                True

                            _ ->
                                False
                    )

        -- ExtraOperand is only relevant if there are no subcommand-related issues
        extraOperandErrors =
            otherReasons
                |> List.filter
                    (\reason ->
                        case reason of
                            ExtraOperand ->
                                True

                            _ ->
                                False
                    )
    in
    if not (List.isEmpty unexpectedOptions) then
        -- Unexpected options - use typo suggestions
        unexpectedOptions
            |> List.map (TypoSuggestion.toMessage parserInfo)
            |> String.join "\n"

    else if not (List.isEmpty missingArgErrors) then
        -- A parser matched the structure but is missing a required argument
        case List.head missingArgErrors of
            Just reason ->
                formatSingleReason reason programName optionsParsers

            Nothing ->
                formatFallbackMessage programName optionsParsers

    else if not (List.isEmpty wrongSubCommandReasons) then
        -- User may have provided an unknown subcommand
        -- But first check: is the "wrong" command actually a valid subcommand?
        -- If so, the error is something else (like ExtraOperand)
        let
            unknownCommands =
                wrongSubCommandReasons
                    |> List.filter (\cmd -> not (List.member cmd availableSubCommands))
        in
        case List.head unknownCommands of
            Just unknownCommand ->
                "Unknown command: `"
                    ++ unknownCommand
                    ++ "`\n\nAvailable commands: "
                    ++ String.join ", " availableSubCommands
                    ++ "\n\nRun with --help for usage information."

            Nothing ->
                -- The command was valid but something else went wrong
                -- Check for ExtraOperand
                if not (List.isEmpty extraOperandErrors) then
                    formatSingleReason ExtraOperand programName optionsParsers

                else
                    formatFallbackMessage programName optionsParsers

    else if hasSubCommandParsers && not (List.isEmpty missingSubCommandReasons) then
        -- Missing subcommand when subcommands are expected
        "Missing command.\n\nAvailable commands: "
            ++ String.join ", " availableSubCommands
            ++ "\n\nRun with --help for usage information."

    else
        -- Format other specific reasons
        case List.head otherReasons of
            Just reason ->
                formatSingleReason reason programName optionsParsers

            Nothing ->
                formatFallbackMessage programName optionsParsers


{-| Format a single NoMatchReason into a message.
-}
formatSingleReason : NoMatchReason -> String -> List (OptionsParser msg BuilderState.NoMoreOptions) -> String
formatSingleReason reason programName optionsParsers =
    case reason of
        UnexpectedOption name ->
            "Unexpected option: --" ++ name

        MissingSubCommand { expectedSubCommand } ->
            "Missing command.\n\nRun with --help for usage information."

        WrongSubCommand { expectedSubCommand, actualSubCommand } ->
            "Unknown command: `" ++ actualSubCommand ++ "`"

        MissingRequiredPositionalArg { name, customMessage } ->
            case customMessage of
                Just message ->
                    message ++ "\n\n" ++ Cli.LowLevel.helpText programName optionsParsers

                Nothing ->
                    "Missing required argument: <" ++ name ++ ">\n\n" ++ Cli.LowLevel.helpText programName optionsParsers

        MissingRequiredKeywordArg { name, customMessage } ->
            case customMessage of
                Just message ->
                    message ++ "\n\n" ++ Cli.LowLevel.helpText programName optionsParsers

                Nothing ->
                    "Missing required option: --" ++ name ++ "\n\n" ++ Cli.LowLevel.helpText programName optionsParsers

        MissingExpectedFlag { name } ->
            "Missing required flag: --" ++ name ++ "\n\n" ++ Cli.LowLevel.helpText programName optionsParsers

        ExtraOperand ->
            "Too many arguments provided.\n\n" ++ Cli.LowLevel.helpText programName optionsParsers


{-| Fallback message when no specific reason is available.
-}
formatFallbackMessage : String -> List (OptionsParser msg BuilderState.NoMoreOptions) -> String
formatFallbackMessage programName optionsParsers =
    "Could not match arguments.\n\nUsage:\n\n" ++ Cli.LowLevel.helpText programName optionsParsers
