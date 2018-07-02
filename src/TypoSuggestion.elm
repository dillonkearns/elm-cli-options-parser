module TypoSuggestion exposing (TypoSuggestion(..), getSuggestions)

import Cli.Command as Command exposing (Command)
import Cli.UsageSpec as UsageSpec
import List.Extra


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


optionSuggestions : List (Command msg) -> List TypoSuggestion
optionSuggestions commands =
    commands
        |> List.map Command.getUsageSpecs
        |> List.concat
        |> List.Extra.uniqueBy UsageSpec.name
        |> List.map UsageSpec.name
        |> List.map Flag
