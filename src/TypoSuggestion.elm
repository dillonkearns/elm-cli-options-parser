module TypoSuggestion exposing (OptionsParser, TypoSuggestion(..), getSuggestions, toMessage)

import Cli.UsageSpec as UsageSpec
import Fuzzy
import List.Extra


type alias OptionsParser =
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


toMessage : List OptionsParser -> String -> String
toMessage optionsParsers unexpectedOption =
    case getSuggestions optionsParsers unexpectedOption |> List.head of
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


getSuggestions : List OptionsParser -> String -> List TypoSuggestion
getSuggestions optionsParsers unexpectedOption =
    let
        something needle hay =
            Fuzzy.match [] [] needle hay |> .score
    in
    (buildSubCommandSuggestions optionsParsers
        ++ optionSuggestions optionsParsers
    )
        |> List.sortBy (name >> something unexpectedOption)


buildSubCommandSuggestions : List OptionsParser -> List TypoSuggestion
buildSubCommandSuggestions optionsParsers =
    optionsParsers
        |> List.filterMap .subCommand
        |> List.map SubCommand


optionSuggestions : List OptionsParser -> List TypoSuggestion
optionSuggestions optionsParsers =
    optionsParsers
        |> List.concatMap .usageSpecs
        |> List.Extra.uniqueBy UsageSpec.name
        |> List.map UsageSpec.name
        |> List.map Flag
