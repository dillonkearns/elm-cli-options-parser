module TypoSuggestion exposing (OptionsParser, TypoSuggestion(..), getSuggestions, toMessage)

import Cli.ColorMode exposing (ColorMode, useColor)
import Cli.Style
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


{-| Format a styled suggestion showing the typo and the suggested correction.
-}
suggestionToStringStyled : ColorMode -> TypoSuggestion -> String
suggestionToStringStyled colorMode typoSuggestion =
    Cli.Style.applyGreen (useColor colorMode)
        ("`"
            ++ (case typoSuggestion of
                    Flag flagName ->
                        "--" ++ flagName

                    SubCommand buildSubCommandName ->
                        buildSubCommandName
               )
            ++ "`"
        )


toMessage : ColorMode -> List OptionsParser -> String -> String
toMessage colorMode optionsParsers unexpectedOption =
    case getSuggestions optionsParsers unexpectedOption |> List.head of
        Just bestSuggestion ->
            "The "
                ++ Cli.Style.applyCyan (useColor colorMode) ("`--" ++ unexpectedOption ++ "`")
                ++ " flag was not found. Maybe it was one of these typos?\n\n"
                ++ Cli.Style.applyCyan (useColor colorMode) ("`--" ++ unexpectedOption ++ "`")
                ++ " <> "
                ++ suggestionToStringStyled colorMode bestSuggestion

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
