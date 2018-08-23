module Cli.UsageSpec exposing
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
    , optionalPositionalArg
    , restArgs
    , synopsis
    )

import List.Extra
import Occurences exposing (Occurences)


type UsageSpec
    = FlagOrKeywordArg FlagOrKeywordArg (Maybe MutuallyExclusiveValues) Occurences
    | Operand String (Maybe MutuallyExclusiveValues) Occurences
    | RestArgs String


type FlagOrKeywordArg
    = Flag String
    | KeywordArg String


type MutuallyExclusiveValues
    = MutuallyExclusiveValues (List String)


keywordArg : String -> Occurences -> UsageSpec
keywordArg keywordArgName occurences =
    FlagOrKeywordArg (KeywordArg keywordArgName) Nothing occurences


flag : String -> Occurences -> UsageSpec
flag flagName occurences =
    FlagOrKeywordArg (Flag flagName) Nothing occurences


operand : String -> UsageSpec
operand operandName =
    Operand operandName Nothing Occurences.Required


optionalPositionalArg : String -> UsageSpec
optionalPositionalArg positionalArgName =
    Operand positionalArgName Nothing Occurences.Optional


restArgs : String -> UsageSpec
restArgs restArgsName =
    RestArgs restArgsName


changeUsageSpec : List String -> UsageSpec -> UsageSpec
changeUsageSpec possibleValues usageSpec =
    case usageSpec of
        FlagOrKeywordArg option mutuallyExclusiveValues occurences ->
            FlagOrKeywordArg option (MutuallyExclusiveValues possibleValues |> Just) occurences

        Operand operandName mutuallyExclusiveValues occurences ->
            Operand operandName (MutuallyExclusiveValues possibleValues |> Just) occurences

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

                    Operand operandName mutuallyExclusiveValues occurences ->
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

                    Operand _ _ _ ->
                        Nothing

                    RestArgs _ ->
                        Nothing
            )
        |> List.Extra.find (\option -> optionName option == thisOptionName)


isOperand : UsageSpec -> Bool
isOperand option =
    case option of
        Operand operandName mutuallyExclusiveValues occurences ->
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
                Flag flagName ->
                    flagName

                KeywordArg keywordArgName ->
                    keywordArgName

        Operand operandOptionName mutuallyExclusiveValues occurences ->
            operandOptionName

        RestArgs restArgsDescription ->
            restArgsDescription


synopsis : String -> { optionsParser | usageSpecs : List UsageSpec, description : Maybe String, subCommand : Maybe String } -> String
synopsis programName { usageSpecs, description, subCommand } =
    programName
        ++ " "
        ++ ((subCommand
                :: (usageSpecs
                        |> List.map
                            (\spec ->
                                (case spec of
                                    FlagOrKeywordArg option mutuallyExclusiveValues occurences ->
                                        optionSynopsis occurences option mutuallyExclusiveValues

                                    Operand operandName mutuallyExclusiveValues occurences ->
                                        let
                                            positionalArgSummary =
                                                mutuallyExclusiveValues
                                                    |> Maybe.map mutuallyExclusiveSynopsis
                                                    |> Maybe.withDefault operandName
                                        in
                                        case occurences of
                                            Occurences.Required ->
                                                "<" ++ positionalArgSummary ++ ">"

                                            Occurences.Optional ->
                                                "[<" ++ positionalArgSummary ++ ">]"

                                            Occurences.ZeroOrMore ->
                                                "TODO shouldn't reach this case"

                                    RestArgs restArgsDescription ->
                                        "<" ++ restArgsDescription ++ ">..."
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
optionSynopsis occurences option maybeMutuallyExclusiveValues =
    (case option of
        Flag flagName ->
            "--" ++ flagName

        KeywordArg keywordArgName ->
            case maybeMutuallyExclusiveValues of
                Just mutuallyExclusiveValues ->
                    "--" ++ keywordArgName ++ " <" ++ mutuallyExclusiveSynopsis mutuallyExclusiveValues ++ ">"

                Nothing ->
                    "--" ++ keywordArgName ++ " <" ++ keywordArgName ++ ">"
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

                        Operand _ _ _ ->
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

                KeywordArg optionName_ ->
                    True

        Nothing ->
            False


optionName : FlagOrKeywordArg -> String
optionName option =
    case option of
        Flag flagName ->
            flagName

        KeywordArg keywordArgName ->
            keywordArgName
