module TypoSuggestion exposing (TypoSuggestion(..), getSuggestions)

import Cli.Command as Command exposing (Command)


type TypoSuggestion
    = Flag String
    | SubCommand String


getSuggestions : List (Command msg) -> String -> List TypoSuggestion
getSuggestions commands string =
    commands
        |> List.map Command.getSubCommand
        |> List.filterMap identity
        |> List.map SubCommand
