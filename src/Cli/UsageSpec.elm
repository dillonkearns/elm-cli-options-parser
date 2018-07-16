module Cli.UsageSpec
    exposing
        ( MutuallyExclusiveValues(MutuallyExclusiveValues)
        , Option(..)
        , UsageSpec
        , changeUsageSpec
        , hasRestArgs
        , isOperand
        , keywordArg
        , name
        , operand
        , operandCount
        , option
        , optionExists
        , optionHasArg
        , restArgs
        , synopsis
        )

import List.Extra
import Occurences exposing (Occurences(..))


type Option
    = Flag String
    | OptionWithStringArg String


type MutuallyExclusiveValues
    = MutuallyExclusiveValues (List String)


type UsageSpec
    = Option Option (Maybe MutuallyExclusiveValues) Occurences
    | Operand String (Maybe MutuallyExclusiveValues)
    | RestArgs String


keywordArg : String -> Occurences -> UsageSpec
keywordArg optionName occurences =
    Option (OptionWithStringArg optionName) Nothing occurences


option : Option -> Occurences -> UsageSpec
option option occurences =
    Option option Nothing occurences


operand : String -> UsageSpec
operand name =
    Operand name Nothing


restArgs : String -> UsageSpec
restArgs name =
    RestArgs name


changeUsageSpec : List String -> UsageSpec -> UsageSpec
changeUsageSpec possibleValues usageSpec =
    case usageSpec of
        Option option mutuallyExclusiveValues occurences ->
            Option option (MutuallyExclusiveValues possibleValues |> Just) occurences

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
                    Option _ _ _ ->
                        Nothing

                    Operand operandName mutuallyExclusiveValues ->
                        Just operandName

                    RestArgs _ ->
                        Nothing
            )
        |> List.length


optionExists : List UsageSpec -> String -> Maybe Option
optionExists usageSpecs thisOptionName =
    usageSpecs
        |> List.filterMap
            (\usageSpec ->
                case usageSpec of
                    Option option mutuallyExclusiveValues occurences ->
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

        Option _ _ _ ->
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
        Option option mutuallyExclusiveValues occurences ->
            case option of
                Flag name ->
                    name

                OptionWithStringArg name ->
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
                                    Option option mutuallyExclusiveValues occurences ->
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


optionSynopsis : Occurences -> Option -> Maybe MutuallyExclusiveValues -> String
optionSynopsis occurences option mutuallyExclusiveValues =
    (case option of
        Flag flagName ->
            "--" ++ flagName

        OptionWithStringArg optionName ->
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
                        Option option mutuallyExclusiveValues occurences ->
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

                OptionWithStringArg optionName ->
                    True

        Nothing ->
            False


optionName : Option -> String
optionName option =
    case option of
        Flag flagName ->
            flagName

        OptionWithStringArg optionName ->
            optionName
