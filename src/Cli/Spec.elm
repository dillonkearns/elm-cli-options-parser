module Cli.Spec exposing (..)

import Cli.Decode
import Cli.UsageSpec exposing (..)
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
                                            ++ [ { name = Cli.UsageSpec.name usageSpec
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
        (Operand operandDescription)
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
        (Option (OptionWithStringArg optionName) Optional)
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
        (Option (OptionWithStringArg optionName) Required)
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
        (Option (Flag flagName) Optional)
        Cli.Decode.decoder


map : (toRaw -> toMapped) -> CliSpec from toRaw -> CliSpec from toMapped
map mapFn (CliSpec dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) =
    CliSpec dataGrabber usageSpec (Cli.Decode.map mapFn decoder)


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
                                            { name = Cli.UsageSpec.name usageSpec
                                            , invalidReason = invalidReason
                                            , valueAsString = toString value
                                            }
                                            |> Err

                            Err error ->
                                Err error
                   )
            )
        )


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
        (Option (OptionWithStringArg flagName) ZeroOrMore)
        Cli.Decode.decoder
