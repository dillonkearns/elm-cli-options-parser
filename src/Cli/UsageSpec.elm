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
    , setDescription
    , synopsis
    )

import List.Extra
import Occurences exposing (Occurences)


type UsageSpec
    = FlagOrKeywordArg FlagOrKeywordArg (Maybe MutuallyExclusiveValues) Occurences (Maybe String)
    | Operand String (Maybe MutuallyExclusiveValues) Occurences (Maybe String)
    | RestArgs String (Maybe String)


type FlagOrKeywordArg
    = Flag String
    | KeywordArg String


type MutuallyExclusiveValues
    = MutuallyExclusiveValues (List String)


keywordArg : String -> Occurences -> UsageSpec
keywordArg keywordArgName occurences =
    FlagOrKeywordArg (KeywordArg keywordArgName) Nothing occurences Nothing


flag : String -> Occurences -> UsageSpec
flag flagName occurences =
    FlagOrKeywordArg (Flag flagName) Nothing occurences Nothing


operand : String -> UsageSpec
operand operandName =
    Operand operandName Nothing Occurences.Required Nothing


optionalPositionalArg : String -> UsageSpec
optionalPositionalArg positionalArgName =
    Operand positionalArgName Nothing Occurences.Optional Nothing


restArgs : String -> UsageSpec
restArgs restArgsName =
    RestArgs restArgsName Nothing


{-| Set the description for a UsageSpec.
-}
setDescription : Maybe String -> UsageSpec -> UsageSpec
setDescription description usageSpec =
    case usageSpec of
        FlagOrKeywordArg option mutuallyExclusiveValues occurences _ ->
            FlagOrKeywordArg option mutuallyExclusiveValues occurences description

        Operand operandName mutuallyExclusiveValues occurences _ ->
            Operand operandName mutuallyExclusiveValues occurences description

        RestArgs restArgsName _ ->
            RestArgs restArgsName description


changeUsageSpec : List String -> UsageSpec -> UsageSpec
changeUsageSpec possibleValues usageSpec =
    case usageSpec of
        FlagOrKeywordArg option _ occurences description ->
            FlagOrKeywordArg option (MutuallyExclusiveValues possibleValues |> Just) occurences description

        Operand operandName _ occurences description ->
            Operand operandName (MutuallyExclusiveValues possibleValues |> Just) occurences description

        _ ->
            usageSpec


operandCount : List UsageSpec -> Int
operandCount usageSpecs =
    usageSpecs
        |> List.filterMap
            (\spec ->
                case spec of
                    FlagOrKeywordArg _ _ _ _ ->
                        Nothing

                    Operand operandName _ _ _ ->
                        Just operandName

                    RestArgs _ _ ->
                        Nothing
            )
        |> List.length


optionExists : List UsageSpec -> String -> Maybe FlagOrKeywordArg
optionExists usageSpecs thisOptionName =
    usageSpecs
        |> List.filterMap
            (\usageSpec ->
                case usageSpec of
                    FlagOrKeywordArg option _ _ _ ->
                        option
                            |> Just

                    Operand _ _ _ _ ->
                        Nothing

                    RestArgs _ _ ->
                        Nothing
            )
        |> List.Extra.find (\option -> optionName option == thisOptionName)


isOperand : UsageSpec -> Bool
isOperand option =
    case option of
        Operand _ _ _ _ ->
            True

        FlagOrKeywordArg _ _ _ _ ->
            False

        RestArgs _ _ ->
            False


hasRestArgs : List UsageSpec -> Bool
hasRestArgs usageSpecs =
    List.any
        (\usageSpec ->
            case usageSpec of
                RestArgs _ _ ->
                    True

                _ ->
                    False
        )
        usageSpecs


name : UsageSpec -> String
name usageSpec =
    case usageSpec of
        FlagOrKeywordArg option _ _ _ ->
            case option of
                Flag flagName ->
                    flagName

                KeywordArg keywordArgName ->
                    keywordArgName

        Operand operandOptionName _ _ _ ->
            operandOptionName

        RestArgs restArgsDescription _ ->
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
                                    FlagOrKeywordArg option mutuallyExclusiveValues occurences _ ->
                                        optionSynopsis occurences option mutuallyExclusiveValues

                                    Operand operandName mutuallyExclusiveValues occurences _ ->
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

                                    RestArgs restArgsDescription _ ->
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
                        FlagOrKeywordArg option _ _ _ ->
                            Just option

                        Operand _ _ _ _ ->
                            Nothing

                        RestArgs _ _ ->
                            Nothing
                )
            |> List.Extra.find
                (\spec -> optionName spec == optionNameToCheck)
    of
        Just option ->
            case option of
                Flag _ ->
                    False

                KeywordArg _ ->
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
