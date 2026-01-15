module Cli.LowLevel exposing (MatchResult(..), helpText, try)

import Cli.Decode
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.OptionsParser.MatchResult as MatchResult exposing (NoMatchReason(..))
import Set exposing (Set)


type MatchResult msg
    = ValidationErrors (List Cli.Decode.ValidationError)
    | NoMatch (List MatchResult.NoMatchReason)
    | Match msg
    | ShowHelp
    | ShowVersion
    | ShowSubcommandHelp String


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
        argsWithoutNodeAndScript =
            argv |> List.drop 2

        -- Check for subcommand-specific help: "subcommand --help" or "--help subcommand"
        hasHelpFlag =
            List.member "--help" argsWithoutNodeAndScript

        availableSubcommands =
            optionsParsers
                |> List.filterMap OptionsParser.getSubCommand

        firstNonFlagArg =
            argsWithoutNodeAndScript
                |> List.filter (\arg -> not (String.startsWith "--" arg))
                |> List.head

        subcommandHelpResult =
            if hasHelpFlag then
                case firstNonFlagArg of
                    Just arg ->
                        if List.member arg availableSubcommands then
                            Just (ShowSubcommandHelp arg)

                        else
                            Nothing

                    Nothing ->
                        Nothing

            else
                Nothing

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
                    (argsWithoutNodeAndScript
                        |> OptionsParser.tryMatch
                    )

        -- Extract UnexpectedOption strings and find the common ones (truly unknown)
        commonUnexpectedOptions : Set String
        commonUnexpectedOptions =
            matchResults
                |> List.map
                    (\matchResult ->
                        case matchResult of
                            MatchResult.NoMatch reasons ->
                                reasons
                                    |> List.filterMap
                                        (\reason ->
                                            case reason of
                                                UnexpectedOption name ->
                                                    Just name

                                                _ ->
                                                    Nothing
                                        )
                                    |> Set.fromList

                            _ ->
                                Set.empty
                    )
                |> intersection

        -- Collect all NoMatchReasons from all parsers
        allNoMatchReasons : List MatchResult.NoMatchReason
        allNoMatchReasons =
            matchResults
                |> List.concatMap
                    (\matchResult ->
                        case matchResult of
                            MatchResult.NoMatch reasons ->
                                reasons

                            _ ->
                                []
                    )

        -- Build the aggregated list of reasons:
        -- 1. Common unexpected options (wrapped back into UnexpectedOption)
        -- 2. All other reasons (deduplicated)
        aggregatedReasons : List MatchResult.NoMatchReason
        aggregatedReasons =
            let
                unexpectedOptionReasons =
                    commonUnexpectedOptions
                        |> Set.toList
                        |> List.map UnexpectedOption

                otherReasons =
                    allNoMatchReasons
                        |> List.filter
                            (\reason ->
                                case reason of
                                    UnexpectedOption _ ->
                                        False

                                    _ ->
                                        True
                            )
                        |> uniqueReasons
            in
            unexpectedOptionReasons ++ otherReasons
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
                        -- Check for subcommand-specific help before returning NoMatch
                        case subcommandHelpResult of
                            Just helpResult ->
                                helpResult

                            Nothing ->
                                NoMatch aggregatedReasons
           )


{-| Remove duplicate reasons (simple deduplication by converting to string and back).
-}
uniqueReasons : List MatchResult.NoMatchReason -> List MatchResult.NoMatchReason
uniqueReasons reasons =
    reasons
        |> List.foldl
            (\reason ( seen, acc ) ->
                let
                    key =
                        reasonToKey reason
                in
                if Set.member key seen then
                    ( seen, acc )

                else
                    ( Set.insert key seen, reason :: acc )
            )
            ( Set.empty, [] )
        |> Tuple.second
        |> List.reverse


{-| Convert a NoMatchReason to a unique string key for deduplication.
-}
reasonToKey : MatchResult.NoMatchReason -> String
reasonToKey reason =
    case reason of
        UnexpectedOption name ->
            "UnexpectedOption:" ++ name

        MissingSubCommand { expectedSubCommand } ->
            "MissingSubCommand:" ++ expectedSubCommand

        WrongSubCommand { expectedSubCommand, actualSubCommand } ->
            "WrongSubCommand:" ++ expectedSubCommand ++ ":" ++ actualSubCommand

        MissingRequiredPositionalArg { name, customMessage } ->
            "MissingRequiredPositionalArg:" ++ name ++ Maybe.withDefault "" customMessage

        MissingRequiredKeywordArg { name, customMessage } ->
            "MissingRequiredKeywordArg:" ++ name ++ Maybe.withDefault "" customMessage

        MissingExpectedFlag { name } ->
            "MissingExpectedFlag:" ++ name

        ExtraOperand ->
            "ExtraOperand"


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
