module Cli.UsageSpec
    exposing
        ( MutuallyExclusiveValues
        , UsageSpec
        , changeUsageSpec
        , flag
        , hasRestArgs
        , isOperand
        , keywordArg
        , name
        , operand
        , operandCount
        , optionExists
        , optionHasArg
        , restArgs
        , synopsis
        )

import List.Extra
import Occurences exposing (Occurences(..))


type UsageSpec
    = FlagOrKeywordArg FlagOrKeywordArg (Maybe MutuallyExclusiveValues) Occurences
    | Operand String (Maybe MutuallyExclusiveValues)
    | RestArgs String


type FlagOrKeywordArg
    = Flag String
    | KeywordArg String


type MutuallyExclusiveValues
    = MutuallyExclusiveValues (List String)


keywordArg : String -> Occurences -> UsageSpec
keywordArg optionName occurences =
    FlagOrKeywordArg (KeywordArg optionName) Nothing occurences


flag : String -> Occurences -> UsageSpec
flag optionName occurences =
    FlagOrKeywordArg (Flag optionName) Nothing occurences


operand : String -> UsageSpec
operand name =
    Operand name Nothing


restArgs : String -> UsageSpec
restArgs name =
    RestArgs name


changeUsageSpec : List String -> UsageSpec -> UsageSpec
changeUsageSpec possibleValues usageSpec =
    case usageSpec of
        FlagOrKeywordArg option mutuallyExclusiveValues occurences ->
            FlagOrKeywordArg option (MutuallyExclusiveValues possibleValues |> Just) occurences

        Operand name mutuallyExclusiveValues ->
            Operand name (MutuallyExclusiveValues possibleValues |> Just)

        _ ->
            usageSpec


operandCount : List UsageSpec -> Int
operandCount usageSpecs =
    usageSpecs
        |> List.filterMap
            (\spec ->
                case spec of
                    FlagOrKeywordArg _ _ _ ->
                        Nothing

                    Operand operandName mutuallyExclusiveValues ->
                        Just operandName

                    RestArgs _ ->
                        Nothing
            )
        |> List.length


optionExists : List UsageSpec -> String -> Maybe FlagOrKeywordArg
optionExists usageSpecs thisOptionName =
    usageSpecs
        |> List.filterMap
            (\usageSpec ->
                case usageSpec of
                    FlagOrKeywordArg option mutuallyExclusiveValues occurences ->
                        option
                            |> Just

                    Operand _ _ ->
                        Nothing

                    RestArgs _ ->
                        Nothing
            )
        |> List.Extra.find (\option -> optionName option == thisOptionName)


isOperand : UsageSpec -> Bool
isOperand option =
    case option of
        Operand operand mutuallyExclusiveValues ->
            True

        FlagOrKeywordArg _ _ _ ->
            False

        RestArgs _ ->
            False


hasRestArgs : List UsageSpec -> Bool
hasRestArgs usageSpecs =
    List.any
        (\usageSpec ->
            case usageSpec of
                RestArgs _ ->
                    True

                _ ->
                    False
        )
        usageSpecs


name : UsageSpec -> String
name usageSpec =
    case usageSpec of
        FlagOrKeywordArg option mutuallyExclusiveValues occurences ->
            case option of
                Flag name ->
                    name

                KeywordArg name ->
                    name

        Operand name mutuallyExclusiveValues ->
            name

        RestArgs name ->
            name


synopsis : String -> { command | usageSpecs : List UsageSpec, description : Maybe String, buildSubCommand : Maybe String } -> String
synopsis programName { usageSpecs, description, buildSubCommand } =
    programName
        ++ " "
        ++ ((buildSubCommand
                :: (usageSpecs
                        |> List.map
                            (\spec ->
                                (case spec of
                                    FlagOrKeywordArg option mutuallyExclusiveValues occurences ->
                                        optionSynopsis occurences option mutuallyExclusiveValues

                                    Operand operandName mutuallyExclusiveValues ->
                                        "<"
                                            ++ (mutuallyExclusiveValues
                                                    |> Maybe.map mutuallyExclusiveSynopsis
                                                    |> Maybe.withDefault operandName
                                               )
                                            ++ ">"

                                    RestArgs description ->
                                        "<" ++ description ++ ">..."
                                )
                                    |> Just
                            )
                   )
            )
                |> List.filterMap identity
                |> String.join " "
           )
        ++ (description |> Maybe.map (\doc -> " # " ++ doc) |> Maybe.withDefault "")


mutuallyExclusiveSynopsis : MutuallyExclusiveValues -> String
mutuallyExclusiveSynopsis (MutuallyExclusiveValues values) =
    String.join "|" values


optionSynopsis : Occurences -> FlagOrKeywordArg -> Maybe MutuallyExclusiveValues -> String
optionSynopsis occurences option mutuallyExclusiveValues =
    (case option of
        Flag flagName ->
            "--" ++ flagName

        KeywordArg optionName ->
            case mutuallyExclusiveValues of
                Just mutuallyExclusiveValues ->
                    "--" ++ optionName ++ " <" ++ mutuallyExclusiveSynopsis mutuallyExclusiveValues ++ ">"

                Nothing ->
                    "--" ++ optionName ++ " <" ++ optionName ++ ">"
    )
        |> Occurences.qualifySynopsis occurences


optionHasArg : List UsageSpec -> String -> Bool
optionHasArg options optionNameToCheck =
    case
        options
            |> List.filterMap
                (\spec ->
                    case spec of
                        FlagOrKeywordArg option mutuallyExclusiveValues occurences ->
                            Just option

                        Operand _ _ ->
                            Nothing

                        RestArgs _ ->
                            Nothing
                )
            |> List.Extra.find
                (\spec -> optionName spec == optionNameToCheck)
    of
        Just option ->
            case option of
                Flag flagName ->
                    False

                KeywordArg optionName ->
                    True

        Nothing ->
            False


optionName : FlagOrKeywordArg -> String
optionName option =
    case option of
        Flag flagName ->
            flagName

        KeywordArg optionName ->
            optionName
