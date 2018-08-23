module Cli.LowLevel exposing (MatchResult(..), helpText, try)

import Cli.Decode
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.OptionsParser.MatchResult as MatchResult exposing (MatchResult)
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


type CombinedParser userOptions
    = SystemParser (MatchResult userOptions)
    | UserParser userOptions


try : List (OptionsParser.OptionsParser msg builderState) -> List String -> MatchResult msg
try optionsParsers argv =
    let
        matchResults =
            (optionsParsers
                |> List.map (OptionsParser.map UserParser)
                |> List.map OptionsParser.end
            )
                ++ [ helpParser
                        |> OptionsParser.end
                        |> OptionsParser.map SystemParser
                   , showVersionParser
                        |> OptionsParser.end
                        |> OptionsParser.map SystemParser
                   ]
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
                                case msg of
                                    SystemParser systemMsg ->
                                        systemMsg

                                    UserParser userMsg ->
                                        Match userMsg

                            Err validationErrors ->
                                ValidationErrors validationErrors

                    Nothing ->
                        NoMatch commonUnmatchedFlags
           )


helpParser : OptionsParser (MatchResult msg) BuilderState.AnyOptions
helpParser =
    OptionsParser.build ShowHelp
        |> OptionsParser.expectFlag "help"


showVersionParser : OptionsParser (MatchResult msg) BuilderState.AnyOptions
showVersionParser =
    OptionsParser.build ShowVersion
        |> OptionsParser.expectFlag "version"


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


helpText : String -> List (OptionsParser msg builderState) -> String
helpText programName optionsParsers =
    optionsParsers
        |> List.map (OptionsParser.synopsis programName)
        |> String.join "\n"
