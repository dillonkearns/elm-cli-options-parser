module Cli.OptionsParser exposing (ExitStatus(..), OptionsParser, RunResult(..), run, runNew)

{-| TODO

@docs RunResult, ExitStatus, OptionsParser
@docs run, runNew

-}

import Cli.Command as Command exposing (Command)
import Cli.LowLevel
import TypoSuggestion


{-| TODO
-}
type RunResult match
    = SystemMessage ExitStatus String
    | CustomMatch match


{-| TODO
-}
type ExitStatus
    = Success
    | Failure


{-| TODO
-}
type alias OptionsParser msg =
    { programName : String
    , commands : List (Command msg)
    , version : String
    }


{-| TODO
-}
run : String -> List (Command msg) -> List String -> RunResult msg
run programName commands =
    runNew
        { programName = programName
        , commands = commands
        , version = "3.1.4"
        }


{-| TODO
-}
runNew : OptionsParser msg -> List String -> RunResult msg
runNew { programName, commands, version } argv =
    let
        matchResult =
            Cli.LowLevel.try commands argv
    in
    case matchResult of
        Cli.LowLevel.NoMatch unexpectedOptions ->
            if unexpectedOptions == [] then
                "\nNo matching command...\n\nUsage:\n\n"
                    ++ Cli.LowLevel.helpText "elm-test" commands
                    |> SystemMessage Failure
            else
                unexpectedOptions
                    |> List.map (TypoSuggestion.toMessage commands)
                    |> String.join "\n"
                    |> SystemMessage Failure

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
                |> SystemMessage Failure

        Cli.LowLevel.Match msg ->
            msg
                |> CustomMatch

        Cli.LowLevel.ShowHelp ->
            Cli.LowLevel.helpText programName commands
                |> SystemMessage Success

        Cli.LowLevel.ShowVersion ->
            programName
                ++ " version "
                ++ version
                |> SystemMessage Success
