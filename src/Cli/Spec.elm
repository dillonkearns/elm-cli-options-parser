module Cli.Spec
    exposing
        ( CliSpec(CliSpec)
        , MutuallyExclusiveValue(MutuallyExclusiveValue)
        , flag
        , keywordArgList
        , map
        , oneOf
        , optionalKeywordArg
        , positionalArg
        , requiredKeywordArg
        , validate
        , validateIfPresent
        , validateMap
        , validateMapMaybe
        , withDefault
        )

import Cli.Decode
import Cli.UsageSpec as UsageSpec exposing (..)
import Cli.Validate as Validate
import List.Extra
import Occurences exposing (Occurences(Optional, Required, ZeroOrMore))
import Parser


type CliSpec from to
    = CliSpec (DataGrabber from) UsageSpec (Cli.Decode.Decoder from to)


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec, operands : List String, options : List Parser.ParsedOption, operandsSoFar : Int } -> Result Cli.Decode.ProcessingError decodesTo


validate : (to -> Validate.ValidationResult) -> CliSpec from to -> CliSpec from to
validate validateFunction (CliSpec dataGrabber usageSpec (Cli.Decode.Decoder decodeFn)) =
    CliSpec dataGrabber
        usageSpec
        (Cli.Decode.Decoder
            (decodeFn
                >> (\result ->
                        Result.map
                            (\( validationErrors, value ) ->
                                case validateFunction value of
                                    Validate.Valid ->
                                        ( validationErrors, value )

                                    Validate.Invalid invalidReason ->
                                        ( validationErrors
                                            ++ [ { name = UsageSpec.name usageSpec
                                                 , invalidReason = invalidReason
                                                 , valueAsString = toString value
                                                 }
                                               ]
                                        , value
                                        )
                            )
                            result
                   )
            )
        )


validateIfPresent : (to -> Validate.ValidationResult) -> CliSpec from (Maybe to) -> CliSpec from (Maybe to)
validateIfPresent validateFunction cliSpec =
    validate
        (\maybeValue ->
            case maybeValue of
                Just value ->
                    validateFunction value

                Nothing ->
                    Validate.Valid
        )
        cliSpec


positionalArg : String -> CliSpec String String
positionalArg operandDescription =
    CliSpec
        (\{ usageSpecs, operands, operandsSoFar } ->
            case
                operands
                    |> List.Extra.getAt operandsSoFar
            of
                Just operandValue ->
                    Ok operandValue

                Nothing ->
                    Cli.Decode.MatchError ("Expect operand " ++ operandDescription ++ "at " ++ toString operandsSoFar ++ " but had operands " ++ toString operands) |> Err
        )
        (UsageSpec.operand operandDescription)
        Cli.Decode.decoder


optionalKeywordArg : String -> CliSpec (Maybe String) (Maybe String)
optionalKeywordArg optionName =
    CliSpec
        (\{ operands, options } ->
            case
                options
                    |> List.Extra.find
                        (\(Parser.ParsedOption thisOptionName optionKind) -> thisOptionName == optionName)
            of
                Nothing ->
                    Ok Nothing

                Just (Parser.ParsedOption _ (Parser.OptionWithArg optionArg)) ->
                    Ok (Just optionArg)

                _ ->
                    Cli.Decode.MatchError ("Expected option " ++ optionName ++ " to have arg but found none.") |> Err
        )
        (UsageSpec.option (OptionWithStringArg optionName) Optional)
        Cli.Decode.decoder


requiredKeywordArg : String -> CliSpec String String
requiredKeywordArg optionName =
    CliSpec
        (\{ operands, options } ->
            case
                options
                    |> List.Extra.find
                        (\(Parser.ParsedOption thisOptionName optionKind) -> thisOptionName == optionName)
            of
                Nothing ->
                    Cli.Decode.MatchError ("Expected to find option " ++ optionName ++ " but only found options " ++ toString options) |> Err

                Just (Parser.ParsedOption _ (Parser.OptionWithArg optionArg)) ->
                    Ok optionArg

                _ ->
                    Cli.Decode.MatchError ("Expected option " ++ optionName ++ " to have arg but found none.") |> Err
        )
        (UsageSpec.option (OptionWithStringArg optionName) Required)
        Cli.Decode.decoder


flag : String -> CliSpec Bool Bool
flag flagName =
    CliSpec
        (\{ options } ->
            if
                options
                    |> List.member (Parser.ParsedOption flagName Parser.Flag)
            then
                Ok True
            else
                Ok False
        )
        (UsageSpec.option (Flag flagName) Optional)
        Cli.Decode.decoder


map : (toRaw -> toMapped) -> CliSpec from toRaw -> CliSpec from toMapped
map mapFn (CliSpec dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) =
    CliSpec dataGrabber usageSpec (Cli.Decode.map mapFn decoder)


type MutuallyExclusiveValue union
    = MutuallyExclusiveValue String union


oneOf : value -> List (MutuallyExclusiveValue value) -> CliSpec from String -> CliSpec from value
oneOf default list (CliSpec dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) =
    validateMap
        (\argValue ->
            case
                list
                    |> List.Extra.find (\(MutuallyExclusiveValue name value) -> name == argValue)
                    |> Maybe.map (\(MutuallyExclusiveValue name value) -> value)
            of
                Nothing ->
                    Err
                        ("Must be one of ["
                            ++ (list
                                    |> List.map (\(MutuallyExclusiveValue name value) -> name)
                                    |> String.join ", "
                               )
                            ++ "]"
                        )

                Just matchingValue ->
                    Ok matchingValue
        )
        (CliSpec dataGrabber
            (UsageSpec.changeUsageSpec
                (list
                    |> List.map (\(MutuallyExclusiveValue name value) -> name)
                )
                usageSpec
            )
            (Cli.Decode.Decoder decodeFn)
        )


validateMap : (to -> Result String toMapped) -> CliSpec from to -> CliSpec from toMapped
validateMap mapFn (CliSpec dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) =
    CliSpec dataGrabber
        usageSpec
        (Cli.Decode.Decoder
            (decodeFn
                >> (\result ->
                        case result of
                            Ok ( validationErrors, value ) ->
                                case mapFn value of
                                    Ok mappedValue ->
                                        Ok ( validationErrors, mappedValue )

                                    Err invalidReason ->
                                        Cli.Decode.UnrecoverableValidationError
                                            { name = UsageSpec.name usageSpec
                                            , invalidReason = invalidReason
                                            , valueAsString = toString value
                                            }
                                            |> Err

                            Err error ->
                                Err error
                   )
            )
        )


validateMapMaybe : (to -> Result String toMapped) -> CliSpec (Maybe from) (Maybe to) -> CliSpec (Maybe from) (Maybe toMapped)
validateMapMaybe mapFn ((CliSpec dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) as cliSpec) =
    validateMap
        (\thing ->
            case thing of
                Just actualThing ->
                    mapFn actualThing
                        |> Result.map Just

                Nothing ->
                    Ok Nothing
        )
        cliSpec


withDefault : to -> CliSpec from (Maybe to) -> CliSpec from to
withDefault defaultValue (CliSpec dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) =
    CliSpec dataGrabber usageSpec (Cli.Decode.map (Maybe.withDefault defaultValue) decoder)


keywordArgList : String -> CliSpec (List String) (List String)
keywordArgList flagName =
    CliSpec
        (\{ options } ->
            options
                |> List.filterMap
                    (\(Parser.ParsedOption optionName optionKind) ->
                        case ( optionName == flagName, optionKind ) of
                            ( False, _ ) ->
                                Nothing

                            ( True, Parser.OptionWithArg optionValue ) ->
                                Just optionValue

                            ( True, _ ) ->
                                -- TODO this should probably be an error
                                Nothing
                    )
                |> Ok
        )
        (UsageSpec.option (OptionWithStringArg flagName) ZeroOrMore)
        Cli.Decode.decoder
