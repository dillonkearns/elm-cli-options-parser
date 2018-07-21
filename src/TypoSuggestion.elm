module TypoSuggestion exposing (Command, TypoSuggestion(..), getSuggestions, toMessage)

import Cli.UsageSpec as UsageSpec
import Fuzzy
import List.Extra


type alias Command =
    { usageSpecs : List UsageSpec.UsageSpec
    , subCommand : Maybe String
    }


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


toMessage : List Command -> String -> String
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


getSuggestions : List Command -> String -> List TypoSuggestion
getSuggestions commands unexpectedOption =
    let
        something needle hay =
            Fuzzy.match [] [] needle hay |> .score
    in
    (buildSubCommandSuggestions commands
        ++ optionSuggestions commands
    )
        |> List.sortBy (name >> something unexpectedOption)


buildSubCommandSuggestions : List Command -> List TypoSuggestion
buildSubCommandSuggestions commands =
    commands
        |> List.map .subCommand
        |> List.filterMap identity
        |> List.map SubCommand


optionSuggestions : List Command -> List TypoSuggestion
optionSuggestions commands =
    commands
        |> List.map .usageSpecs
        |> List.concat
        |> List.Extra.uniqueBy UsageSpec.name
        |> List.map UsageSpec.name
        |> List.map Flag
