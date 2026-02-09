module Cli.UsageSpec exposing
    ( MutuallyExclusiveValues
    , UsageSpec
    , changeUsageSpec
    , detailedHelp
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

import Ansi.String
import Cli.ColorMode exposing (ColorMode, useColor)
import Cli.Style
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
        |> List.Extra.findMap
            (\usageSpec ->
                case usageSpec of
                    FlagOrKeywordArg option _ _ _ ->
                        if optionName option == thisOptionName then
                            Just option

                        else
                            Nothing

                    Operand _ _ _ _ ->
                        Nothing

                    RestArgs _ _ ->
                        Nothing
            )


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


synopsis : ColorMode -> String -> { optionsParser | usageSpecs : List UsageSpec, description : Maybe String, subCommand : Maybe String } -> String
synopsis colorMode programName { usageSpecs, description, subCommand } =
    let
        specStrings =
            usageSpecs |> List.map (specToSynopsis colorMode)

        allParts =
            case subCommand of
                Just sub ->
                    sub :: specStrings

                Nothing ->
                    specStrings
    in
    Cli.Style.applyBold (useColor colorMode) programName
        ++ " "
        ++ String.join " " allParts
        ++ (description |> Maybe.map (\doc -> " # " ++ doc) |> Maybe.withDefault "")


{-| Generate detailed help text with Usage line and Options section.
-}
detailedHelp : ColorMode -> String -> { optionsParser | usageSpecs : List UsageSpec, description : Maybe String, subCommand : Maybe String } -> String
detailedHelp colorMode programName ({ usageSpecs, description } as optionsParser) =
    let
        usageLine =
            Cli.Style.applyBold (useColor colorMode) "Usage:"
                ++ " "
                ++ synopsisLine colorMode programName optionsParser

        descriptionSection =
            description
                |> Maybe.map (\doc -> "\n\n" ++ doc)
                |> Maybe.withDefault ""

        optionsWithDescriptions =
            usageSpecs
                |> List.filterMap
                    (\spec ->
                        case spec of
                            FlagOrKeywordArg option mutuallyExclusiveValues _ maybeDesc ->
                                maybeDesc
                                    |> Maybe.map
                                        (\desc ->
                                            ( optionSynopsisForHelp colorMode option mutuallyExclusiveValues
                                            , desc
                                            )
                                        )

                            Operand operandName _ _ maybeDesc ->
                                maybeDesc
                                    |> Maybe.map
                                        (\desc ->
                                            ( Cli.Style.applyCyan (useColor colorMode) ("<" ++ operandName ++ ">")
                                            , desc
                                            )
                                        )

                            RestArgs restArgsName maybeDesc ->
                                maybeDesc
                                    |> Maybe.map
                                        (\desc ->
                                            ( Cli.Style.applyCyan (useColor colorMode) ("<" ++ restArgsName ++ ">...")
                                            , desc
                                            )
                                        )
                    )

        optionsSection =
            if List.isEmpty optionsWithDescriptions then
                ""

            else
                let
                    maxOptionLength =
                        optionsWithDescriptions
                            |> List.map (Tuple.first >> Ansi.String.width)
                            |> List.maximum
                            |> Maybe.withDefault 0

                    padding optionStr =
                        String.repeat (maxOptionLength - Ansi.String.width optionStr + 3) " "

                    formatOption ( optionStr, desc ) =
                        "  " ++ optionStr ++ padding optionStr ++ desc
                in
                "\n\n"
                    ++ Cli.Style.applyBold (useColor colorMode) "Options:"
                    ++ "\n"
                    ++ (optionsWithDescriptions
                            |> List.map formatOption
                            |> String.join "\n"
                       )
    in
    usageLine ++ descriptionSection ++ optionsSection


{-| Generate synopsis line without "Usage:" prefix or description suffix.
-}
synopsisLine : ColorMode -> String -> { optionsParser | usageSpecs : List UsageSpec, description : Maybe String, subCommand : Maybe String } -> String
synopsisLine colorMode programName { usageSpecs, subCommand } =
    let
        specStrings =
            usageSpecs |> List.map (specToSynopsis colorMode)

        allParts =
            case subCommand of
                Just sub ->
                    sub :: specStrings

                Nothing ->
                    specStrings
    in
    Cli.Style.applyBold (useColor colorMode) programName
        ++ " "
        ++ String.join " " allParts


{-| Generate option synopsis for help text (without occurrence brackets).
-}
optionSynopsisForHelp : ColorMode -> FlagOrKeywordArg -> Maybe MutuallyExclusiveValues -> String
optionSynopsisForHelp colorMode option maybeMutuallyExclusiveValues =
    case option of
        Flag flagName ->
            Cli.Style.applyCyan (useColor colorMode) ("--" ++ flagName)

        KeywordArg keywordArgName ->
            case maybeMutuallyExclusiveValues of
                Just mutuallyExclusiveValues ->
                    Cli.Style.applyCyan (useColor colorMode) ("--" ++ keywordArgName ++ " <" ++ mutuallyExclusiveSynopsis mutuallyExclusiveValues ++ ">")

                Nothing ->
                    Cli.Style.applyCyan (useColor colorMode) ("--" ++ keywordArgName ++ " <" ++ keywordArgName ++ ">")


{-| Convert a UsageSpec to its synopsis string representation.
-}
specToSynopsis : ColorMode -> UsageSpec -> String
specToSynopsis colorMode spec =
    case spec of
        FlagOrKeywordArg option mutuallyExclusiveValues occurences _ ->
            optionSynopsisStyled colorMode occurences option mutuallyExclusiveValues

        Operand operandName mutuallyExclusiveValues occurences _ ->
            let
                positionalArgSummary =
                    mutuallyExclusiveValues
                        |> Maybe.map mutuallyExclusiveSynopsis
                        |> Maybe.withDefault operandName
            in
            case occurences of
                Occurences.Required ->
                    Cli.Style.applyCyan (useColor colorMode) ("<" ++ positionalArgSummary ++ ">")

                Occurences.Optional ->
                    "[" ++ Cli.Style.applyCyan (useColor colorMode) ("<" ++ positionalArgSummary ++ ">") ++ "]"

                Occurences.ZeroOrMore ->
                    "TODO shouldn't reach this case"

        RestArgs restArgsDescription _ ->
            Cli.Style.applyCyan (useColor colorMode) ("<" ++ restArgsDescription ++ ">...")


mutuallyExclusiveSynopsis : MutuallyExclusiveValues -> String
mutuallyExclusiveSynopsis (MutuallyExclusiveValues values) =
    String.join "|" values


{-| Generate styled option synopsis with occurrence brackets.
-}
optionSynopsisStyled : ColorMode -> Occurences -> FlagOrKeywordArg -> Maybe MutuallyExclusiveValues -> String
optionSynopsisStyled colorMode occurences option maybeMutuallyExclusiveValues =
    let
        styledOption =
            case option of
                Flag flagName ->
                    Cli.Style.applyCyan (useColor colorMode) ("--" ++ flagName)

                KeywordArg keywordArgName ->
                    case maybeMutuallyExclusiveValues of
                        Just mutuallyExclusiveValues ->
                            Cli.Style.applyCyan (useColor colorMode) ("--" ++ keywordArgName ++ " <" ++ mutuallyExclusiveSynopsis mutuallyExclusiveValues ++ ">")

                        Nothing ->
                            Cli.Style.applyCyan (useColor colorMode) ("--" ++ keywordArgName ++ " <" ++ keywordArgName ++ ">")
    in
    case occurences of
        Occurences.Required ->
            styledOption

        Occurences.Optional ->
            "[" ++ styledOption ++ "]"

        Occurences.ZeroOrMore ->
            "[" ++ styledOption ++ "]..."


optionHasArg : List UsageSpec -> String -> Bool
optionHasArg options optionNameToCheck =
    case
        options
            |> List.Extra.findMap
                (\spec ->
                    case spec of
                        FlagOrKeywordArg option _ _ _ ->
                            if optionName option == optionNameToCheck then
                                Just option

                            else
                                Nothing

                        Operand _ _ _ _ ->
                            Nothing

                        RestArgs _ _ ->
                            Nothing
                )
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
