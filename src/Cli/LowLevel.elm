module Cli.LowLevel exposing (MatchResult(..), helpText, try)

import Cli.Command as Command exposing (ActualCommand)
import Cli.Command.MatchResult as MatchResult exposing (MatchResult)
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


try : List (Command.ActualCommand msg builderStatus) -> List String -> MatchResult msg
try commands argv =
    let
        maybeShowHelpMatch : Maybe (MatchResult msg)
        maybeShowHelpMatch =
            Command.build ShowHelp
                |> Command.expectFlag "help"
                |> Command.end
                |> Command.tryMatch (argv |> List.drop 2)
                |> (\matchResult ->
                        case matchResult of
                            MatchResult.NoMatch _ ->
                                Nothing

                            MatchResult.Match _ ->
                                Just ShowHelp
                   )

        maybeShowVersionMatch : Maybe (MatchResult msg)
        maybeShowVersionMatch =
            Command.build ShowVersion
                |> Command.expectFlag "version"
                |> Command.end
                |> Command.tryMatch (argv |> List.drop 2)
                |> (\matchResult ->
                        case matchResult of
                            MatchResult.NoMatch _ ->
                                Nothing

                            MatchResult.Match _ ->
                                Just ShowVersion
                   )

        matchResults =
            commands
                |> List.map
                    (argv
                        |> List.drop 2
                        |> Command.tryMatch
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


helpText : String -> List (ActualCommand msg builderStatus) -> String
helpText programName commands =
    commands
        |> List.map (Command.synopsis programName)
        |> String.join "\n"
