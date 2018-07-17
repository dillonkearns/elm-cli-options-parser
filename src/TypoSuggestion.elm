module TypoSuggestion exposing (TypoSuggestion(..), getSuggestions, toMessage)

import Cli.Command as Command exposing (Command)
import Cli.UsageSpec as UsageSpec
import Fuzzy
import List.Extra


type TypoSuggestion
    = Flag String
    | SubCommand String


suggestionToString : TypoSuggestion -> String
suggestionToString typoSuggestion =
    "`"
        ++ (case typoSuggestion of
                Flag flagName ->
                    "--" ++ flagName

                SubCommand buildSubCommandName ->
                    buildSubCommandName
           )
        ++ "`"


toMessage : List (Command msg) -> String -> String
toMessage commands unexpectedOption =
    case getSuggestions commands unexpectedOption |> List.head of
        Just bestSuggestion ->
            "The `--"
                ++ unexpectedOption
                ++ "` flag was not found. Maybe it was one of these typos?\n\n`--"
                ++ unexpectedOption
                ++ "` <> "
                ++ suggestionToString bestSuggestion

        Nothing ->
            "TODO"


name : TypoSuggestion -> String
name typoSuggestion =
    case typoSuggestion of
        Flag suggestionName ->
            suggestionName

        SubCommand suggestionName ->
            suggestionName


getSuggestions : List (Command msg) -> String -> List TypoSuggestion
getSuggestions commands unexpectedOption =
    let
        something needle hay =
            Fuzzy.match [] [] needle hay |> .score
    in
    (buildSubCommandSuggestions commands
        ++ optionSuggestions commands
    )
        |> List.sortBy (name >> something unexpectedOption)


buildSubCommandSuggestions : List (Command msg) -> List TypoSuggestion
buildSubCommandSuggestions commands =
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
