module Cli.OptionsParser exposing (Program, RunResult(..), add, program, run)

{-| TODO

@docs RunResult, Program
@docs run
@docs add
@docs program

-}

import Cli.Command as Command exposing (ActualCommand, Command)
import Cli.Command.BuilderState as BuilderState
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
    , commands : List (ActualCommand msg BuilderState.Terminal)
    , version : String
    }


{-| -}
program : { programName : String, version : String } -> Program decodesTo
program { programName, version } =
    { programName = programName
    , version = version
    , commands = []
    }


{-| -}
add : ActualCommand msg anything -> Program msg -> Program msg
add command ({ commands } as program) =
    { program
        | commands = commands ++ [ Command.end command ]
    }


{-| Run an OptionsParser.Program. See the `examples` folder for end-to-end examples.

    type GitCommand
        = Init
        | Clone String

    cli : Cli.OptionsParser.Program GitCommand
    cli =
        { programName = "git"
        , commands = commands
        , version = "1.2.3"
        }

    commands : List (Command.Command GitCommand)
    commands =
        [ Command.buildSubCommand "clone" Clone
            |> with (Cli.Option.positionalArg "repository")
            |> Command.end
        ]

    argv : List String
    argv =
        [{- passed in as Flags from JavaScript, see `examples` folder. -}]

    matchResult : Cli.OptionsParser.RunResult GitCommand
    matchResult =
        Cli.OptionsParser.run cli argv

-}
run : Program msg -> List String -> RunResult msg
run { programName, commands, version } argv =
    let
        matchResult =
            Cli.LowLevel.try commands argv
    in
    case matchResult of
        Cli.LowLevel.NoMatch unexpectedOptions ->
            if unexpectedOptions == [] then
                "\nNo matching command...\n\nUsage:\n\n"
                    ++ Cli.LowLevel.helpText "elm-test" commands
                    |> SystemMessage Cli.ExitStatus.Failure
            else
                unexpectedOptions
                    |> List.map
                        (TypoSuggestion.toMessage
                            (commands
                                |> List.map
                                    (\command ->
                                        { usageSpecs = Command.getUsageSpecs command
                                        , subCommand = Command.getSubCommand command
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
            Cli.LowLevel.helpText programName commands
                |> SystemMessage Cli.ExitStatus.Success

        Cli.LowLevel.ShowVersion ->
            programName
                ++ " version "
                ++ version
                |> SystemMessage Cli.ExitStatus.Success
