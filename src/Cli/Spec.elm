module Cli.Spec exposing (..)

-- (CliSpec(..), positionalArg)

import Cli.Decode
import Cli.UsageSpec exposing (..)
import List.Extra
import Occurences exposing (Occurences(Optional, Required, ZeroOrMore))
import Parser


type CliSpec from to
    = CliSpec (DataGrabber from) UsageSpec (Cli.Decode.Decoder from to)


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec, operands : List String, options : List Parser.ParsedOption, operandsSoFar : Int } -> Result String decodesTo


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
                    Err ("Expect operand " ++ operandDescription ++ "at " ++ toString operandsSoFar ++ " but had operands " ++ toString operands)
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
                    Err ("Expected option " ++ optionName ++ " to have arg but found none.")
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
                    Err ("Expected to find option " ++ optionName ++ " but only found options " ++ toString options)

                Just (Parser.ParsedOption _ (Parser.OptionWithArg optionArg)) ->
                    Ok optionArg

                _ ->
                    Err ("Expected option " ++ optionName ++ " to have arg but found none.")
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


mapNew : (toRaw -> toMapped) -> CliSpec from toRaw -> CliSpec from toMapped
mapNew mapFn (CliSpec dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) =
    CliSpec dataGrabber usageSpec (Cli.Decode.map mapFn decoder)


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
