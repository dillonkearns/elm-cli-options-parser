module Cli exposing (ExecuteResult(..), ExitStatus(..), MatchResult(..), execute, helpText, try)

import Cli.Command as Command exposing (Command)
import Cli.Decode
import Set exposing (Set)
import TypoSuggestion


type ExecuteResult match
    = SystemMessage ExitStatus String
    | CustomMatch match


type ExitStatus
    = Success
    | Failure


execute : List (Command msg) -> List String -> ExecuteResult msg
execute cli flags =
    let
        matchResult =
            try cli flags
    in
    case matchResult of
        NoMatch unexpectedOptions ->
            if unexpectedOptions == [] then
                "\nNo matching command...\n\nUsage:\n\n"
                    ++ helpText "elm-test" cli
                    |> SystemMessage Failure
            else
                unexpectedOptions
                    |> List.map (TypoSuggestion.toMessage cli)
                    |> String.join "\n"
                    |> SystemMessage Failure

        ValidationErrors validationErrors ->
            ("Validation errors:\n\n"
                ++ (validationErrors
                        |> List.map
                            (\{ name, invalidReason, valueAsString } ->
                                "`"
                                    ++ name
                                    ++ "` failed a validation. "
                                    ++ invalidReason
                                    ++ "\nValue was:\n"
                                    ++ valueAsString
                            )
                        |> String.join "\n"
                   )
            )
                |> SystemMessage Failure

        Match msg ->
            msg
                |> CustomMatch


type MatchResult msg
    = ValidationErrors (List Cli.Decode.ValidationError)
    | NoMatch (List String)
    | Match msg


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


try : List (Command msg) -> List String -> MatchResult msg
try commands argv =
    let
        matchResults =
            commands
                |> List.map
                    (argv
                        |> List.drop 2
                        |> Command.tryMatchNew
                    )

        commonUnmatchedFlags =
            matchResults
                |> List.map
                    (\matchResult ->
                        case matchResult of
                            Command.NoMatch unknownFlags ->
                                Set.fromList unknownFlags

                            _ ->
                                Set.empty
                    )
                |> intersection
                |> Set.toList
    in
    matchResults
        |> List.map Command.matchResultToMaybe
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
                        NoMatch commonUnmatchedFlags
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


helpText : String -> List (Command msg) -> String
helpText programName commands =
    commands
        |> List.map (Command.synopsis programName)
        |> String.join "\n"
