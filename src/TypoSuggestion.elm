module TypoSuggestion exposing (TypoSuggestion(..), getSuggestions)

import Cli.Command as Command exposing (Command)


type TypoSuggestion
    = Flag String
    | SubCommand String


getSuggestions : List (Command msg) -> String -> List TypoSuggestion
getSuggestions commands unexpectedOption =
    subCommandSuggestions commands


subCommandSuggestions : List (Command msg) -> List TypoSuggestion
subCommandSuggestions commands =
    commands
        |> List.map Command.getSubCommand
        |> List.filterMap identity
        |> List.map SubCommand
