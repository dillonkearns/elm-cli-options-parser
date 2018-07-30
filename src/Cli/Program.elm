module Cli.Program exposing (Program, ProgramNew, RunResult(..), add, program, programNew, run)

{-| TODO

@docs RunResult, Program, ProgramNew
@docs run
@docs add
@docs program, programNew

-}

import Cli.ExitStatus exposing (ExitStatus)
import Cli.LowLevel
import Cli.OptionsParser as OptionsParser exposing (ActualOptionsParser, OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import TypoSuggestion


{-| TODO
-}
type RunResult match
    = SystemMessage ExitStatus String
    | CustomMatch match


{-| TODO
-}
type alias Program msg =
    { programName : String
    , optionsParsers : List (ActualOptionsParser msg BuilderState.Terminal)
    , version : String
    }


{-| -}
program : { programName : String, version : String } -> Program decodesTo
program { programName, version } =
    { programName = programName
    , version = version
    , optionsParsers = []
    }


{-| TODO
-}
type alias ProgramNew decodesTo =
    Platform.Program (List String) () decodesTo


{-| -}
programNew : (options -> Cmd msg) -> Program options -> ProgramOptions msg -> ProgramNew msg
programNew matchFunction program options =
    Platform.programWithFlags
        { init = init matchFunction options program
        , update = \msg model -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


type alias ProgramOptions msg =
    { printAndExitFailure : String -> Cmd msg
    , printAndExitSuccess : String -> Cmd msg
    }


init : (options -> Cmd msg) -> ProgramOptions msg -> Program options -> List String -> ( (), Cmd msg )
init match options program argv =
    let
        matchResult : RunResult options
        matchResult =
            run program argv

        cmd =
            case matchResult of
                SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.ExitStatus.Failure ->
                            options.printAndExitFailure message

                        Cli.ExitStatus.Success ->
                            options.printAndExitSuccess message

                CustomMatch msg ->
                    match msg
    in
    ( (), cmd )


{-| -}
add : ActualOptionsParser msg anything -> Program msg -> Program msg
add optionsParser ({ optionsParsers } as program) =
    { program
        | optionsParsers = optionsParsers ++ [ OptionsParser.end optionsParser ]
    }


{-| Run an Program.Program. See the `examples` folder for end-to-end examples.

    type GitOptionsParser
        = Init
        | Clone String

    cli : Cli.Program.Program GitOptionsParser
    cli =
        { programName = "git"
        , optionsParsers = optionsParsers
        , version = "1.2.3"
        }

    optionsParsers : List (OptionsParser.OptionsParser GitOptionsParser)
    optionsParsers =
        [ OptionsParser.buildSubCommand "clone" Clone
            |> with (Cli.Option.positionalArg "repository")
            |> OptionsParser.end
        ]

    argv : List String
    argv =
        [{- passed in as Flags from JavaScript, see `examples` folder. -}]

    matchResult : Cli.Program.RunResult GitOptionsParser
    matchResult =
        Cli.Program.run cli argv

-}
run : Program msg -> List String -> RunResult msg
run { programName, optionsParsers, version } argv =
    let
        matchResult =
            Cli.LowLevel.try optionsParsers argv
    in
    case matchResult of
        Cli.LowLevel.NoMatch unexpectedOptions ->
            if unexpectedOptions == [] then
                "\nNo matching optionsParser...\n\nUsage:\n\n"
                    ++ Cli.LowLevel.helpText "elm-test" optionsParsers
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
                            (\{ name, invalidReason, valueAsString } ->
                                "`"
                                    ++ name
                                    ++ "` failed a validation. "
                                    ++ invalidReason
                                    ++ "\nValue was:\n"
                                    ++ valueAsString
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
            programName
                ++ " version "
                ++ version
                |> SystemMessage Cli.ExitStatus.Success
