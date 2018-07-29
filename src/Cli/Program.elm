module Cli.Program exposing (Program, RunResult(..), add, program, run)

{-| TODO

@docs RunResult, Program
@docs run
@docs add
@docs program

-}

import Cli.OptionsParser as OptionsParser exposing (ActualOptionsParser, OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.ExitStatus exposing (ExitStatus)
import Cli.LowLevel
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
