module Cli.Program exposing
    ( config, Config, add
    , stateless, stateful
    , StatelessProgram, StatefulProgram
    , FlagsIncludingArgv
    )

{-|


## Config

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

@docs stateless, stateful
@docs StatelessProgram, StatefulProgram
@docs FlagsIncludingArgv

-}

import Cli.ExitStatus exposing (ExitStatus)
import Cli.LowLevel
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import List.Extra
import TypoSuggestion


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


{-| -}
type alias StatelessProgram msg flags =
    Platform.Program (FlagsIncludingArgv flags) () msg


{-| -}
stateless : ProgramOptions msg options flags -> StatelessProgram msg flags
stateless options =
    Platform.worker
        { init = init options
        , update = \msg model -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


{-| -}
type alias StatefulProgram model msg cliOptions flags =
    Platform.Program (FlagsIncludingArgv flags) (StatefulProgramModel model cliOptions) msg


type alias StatefulOptions msg model cliOptions flags =
    { printAndExitFailure : String -> Cmd msg
    , printAndExitSuccess : String -> Cmd msg
    , init : FlagsIncludingArgv flags -> cliOptions -> ( model, Cmd msg )
    , update : cliOptions -> msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
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
                        options.subscriptions actualModel

                    ShowSystemMessage ->
                        Sub.none
        }


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

        cmd =
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
    in
    cmd


run : Config msg -> List String -> String -> RunResult msg
run (Config { optionsParsers }) argv versionMessage =
    let
        programName =
            case argv of
                first :: programPath :: _ ->
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
    in
    case matchResult of
        Cli.LowLevel.NoMatch unexpectedOptions ->
            if unexpectedOptions == [] then
                "\nNo matching optionsParser...\n\nUsage:\n\n"
                    ++ Cli.LowLevel.helpText programName optionsParsers
                    |> SystemMessage Cli.ExitStatus.Failure

            else
                unexpectedOptions
                    |> List.map
                        (TypoSuggestion.toMessage
                            (optionsParsers
                                |> List.map
                                    (\optionsParser ->
                                        { usageSpecs = OptionsParser.getUsageSpecs optionsParser
                                        , subCommand = OptionsParser.getSubCommand optionsParser
                                        }
                                    )
                            )
                        )
                    |> String.join "\n"
                    |> SystemMessage Cli.ExitStatus.Failure

        Cli.LowLevel.ValidationErrors validationErrors ->
            ("Validation errors:\n\n"
                ++ (validationErrors
                        |> List.map
                            (\{ name, invalidReason } ->
                                "`"
                                    ++ name
                                    ++ "` failed a validation. "
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
            Cli.LowLevel.helpText programName optionsParsers
                |> SystemMessage Cli.ExitStatus.Success

        Cli.LowLevel.ShowVersion ->
            versionMessage
                |> SystemMessage Cli.ExitStatus.Success
