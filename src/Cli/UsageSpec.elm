module Cli.UsageSpec exposing (Option(..), UsageSpec(..), name, optionHasArg, synopsis)

import List.Extra
import Occurences exposing (Occurences(..))


type Option
    = Flag String
    | OptionWithStringArg String


type UsageSpec
    = Option Option Occurences
    | Operand String
    | RestArgs String


name : UsageSpec -> String
name usageSpec =
    case usageSpec of
        Option option occurences ->
            case option of
                Flag name ->
                    name

                OptionWithStringArg name ->
                    name

        Operand name ->
            name

        RestArgs name ->
            name


synopsis : String -> { command | usageSpecs : List UsageSpec, description : Maybe String } -> String
synopsis programName { usageSpecs, description } =
    programName
        ++ " "
        ++ (usageSpecs
                |> List.map
                    (\spec ->
                        case spec of
                            Option option occurences ->
                                optionSynopsis occurences option

                            Operand operandName ->
                                "<" ++ operandName ++ ">"

                            RestArgs description ->
                                "<" ++ description ++ ">..."
                    )
                |> String.join " "
           )
        ++ (description |> Maybe.map (\doc -> " # " ++ doc) |> Maybe.withDefault "")


optionSynopsis : Occurences -> Option -> String
optionSynopsis occurences option =
    (case option of
        Flag flagName ->
            "--" ++ flagName

        OptionWithStringArg optionName ->
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
                        Option option occurences ->
                            Just option

                        Operand _ ->
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
