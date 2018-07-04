module Cli.UsageSpec exposing (MutuallyExclusiveValues(MutuallyExclusiveValues), Option(..), UsageSpec(..), name, optionHasArg, synopsis)

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


name : UsageSpec -> String
name usageSpec =
    case usageSpec of
        Option option oneOf occurences ->
            case option of
                Flag name ->
                    name

                OptionWithStringArg name ->
                    name

        Operand name oneOf ->
            name

        RestArgs name ->
            name


synopsis : String -> { command | usageSpecs : List UsageSpec, description : Maybe String, subCommand : Maybe String } -> String
synopsis programName { usageSpecs, description, subCommand } =
    programName
        ++ " "
        ++ (subCommand |> Maybe.withDefault "")
        ++ (usageSpecs
                |> List.map
                    (\spec ->
                        case spec of
                            Option option oneOf occurences ->
                                optionSynopsis occurences option oneOf

                            Operand operandName oneOf ->
                                "<" ++ operandName ++ ">"

                            RestArgs description ->
                                "<" ++ description ++ ">..."
                    )
                |> String.join " "
           )
        ++ (description |> Maybe.map (\doc -> " # " ++ doc) |> Maybe.withDefault "")


optionSynopsis : Occurences -> Option -> Maybe MutuallyExclusiveValues -> String
optionSynopsis occurences option oneOf =
    (case option of
        Flag flagName ->
            "--" ++ flagName

        OptionWithStringArg optionName ->
            case oneOf of
                Just (MutuallyExclusiveValues oneOf) ->
                    "--" ++ optionName ++ " <" ++ (oneOf |> String.join "|") ++ ">"

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
                        Option option oneOf occurences ->
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
