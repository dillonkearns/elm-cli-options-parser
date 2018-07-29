module Cli.LowLevel exposing (MatchResult(..), helpText, try)

import Cli.OptionsParser as OptionsParser exposing (ActualOptionsParser)
import Cli.OptionsParser.MatchResult as MatchResult exposing (MatchResult)
import Cli.Decode
import Maybe.Extra
import Set exposing (Set)


type MatchResult msg
    = ValidationErrors (List Cli.Decode.ValidationError)
    | NoMatch (List String)
    | Match msg
    | ShowHelp
    | ShowVersion


intersection : List (Set comparable) -> Set comparable
intersection sets =
    case sets of
        [] ->
            Set.empty

        [ set ] ->
            set

        first :: rest ->
            intersection rest
                |> Set.intersect first


try : List (OptionsParser.ActualOptionsParser msg builderStatus) -> List String -> MatchResult msg
try optionsParsers argv =
    let
        maybeShowHelpMatch : Maybe (MatchResult msg)
        maybeShowHelpMatch =
            OptionsParser.build ShowHelp
                |> OptionsParser.expectFlag "help"
                |> OptionsParser.end
                |> OptionsParser.tryMatch (argv |> List.drop 2)
                |> (\matchResult ->
                        case matchResult of
                            MatchResult.NoMatch _ ->
                                Nothing

                            MatchResult.Match _ ->
                                Just ShowHelp
                   )

        maybeShowVersionMatch : Maybe (MatchResult msg)
        maybeShowVersionMatch =
            OptionsParser.build ShowVersion
                |> OptionsParser.expectFlag "version"
                |> OptionsParser.end
                |> OptionsParser.tryMatch (argv |> List.drop 2)
                |> (\matchResult ->
                        case matchResult of
                            MatchResult.NoMatch _ ->
                                Nothing

                            MatchResult.Match _ ->
                                Just ShowVersion
                   )

        matchResults =
            optionsParsers
                |> List.map
                    (argv
                        |> List.drop 2
                        |> OptionsParser.tryMatch
                    )

        commonUnmatchedFlags =
            matchResults
                |> List.map
                    (\matchResult ->
                        case matchResult of
                            MatchResult.NoMatch unknownFlags ->
                                Set.fromList unknownFlags

                            _ ->
                                Set.empty
                    )
                |> intersection
                |> Set.toList
    in
    matchResults
        |> List.map MatchResult.matchResultToMaybe
        |> oneOf
        |> (\maybeResult ->
                case maybeResult of
                    Just result ->
                        case result of
                            Ok msg ->
                                Match msg

                            Err validationErrors ->
                                ValidationErrors validationErrors

                    Nothing ->
                        maybeShowHelpMatch
                            |> Maybe.Extra.or maybeShowVersionMatch
                            |> Maybe.withDefault
                                (NoMatch commonUnmatchedFlags)
           )


oneOf : List (Maybe a) -> Maybe a
oneOf =
    List.foldl
        (\x acc ->
            if acc /= Nothing then
                acc
            else
                x
        )
        Nothing


helpText : String -> List (ActualOptionsParser msg builderStatus) -> String
helpText programName optionsParsers =
    optionsParsers
        |> List.map (OptionsParser.synopsis programName)
        |> String.join "\n"
