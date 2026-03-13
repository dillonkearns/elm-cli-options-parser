module Cli.Option.Internal exposing
    ( DataGrabber
    , InnerOption
    , JsonGrabber
    , Option(..)
    , OptionMeta
    , emptyMeta
    , flagGrabber
    , jsonFieldGrabber
    , jsonFlagGrabber
    , jsonOptionalFieldGrabber
    , jsonOptionalFieldGrabberWithDefault
    , keywordArgListGrabber
    , optionalKeywordArgGrabber
    , optionalPositionalArgGrabber
    , requiredKeywordArgGrabber
    , requiredPositionalArgGrabber
    , restArgsGrabber
    )

import Cli.Decode
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Json.Decode
import List.Extra
import Tokenizer
import TsJson.Type


type Option from to constraints
    = Option (InnerOption from to)


type alias InnerOption from to =
    { dataGrabber : DataGrabber from
    , usageSpec : UsageSpec
    , decoder : Cli.Decode.Decoder from to
    , meta : OptionMeta
    , tsType : TsJson.Type.Type
    , jsonGrabber : JsonGrabber to
    }


{-| Extracts a decoded value from a JSON blob for JSON input mode.
Produces the final `to` type (after all validation/mapping).
-}
type alias JsonGrabber to =
    Json.Decode.Value -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, to )


{-| Metadata for an option that can be set via withMissingMessage.
-}
type alias OptionMeta =
    { missingMessage : Maybe String
    }


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec
    , operands : List String
    , options : List Tokenizer.ParsedOption
    , operandsSoFar : Int
    }
    -> Result Cli.Decode.ProcessingError decodesTo


{-| Default empty metadata.
-}
emptyMeta : OptionMeta
emptyMeta =
    { missingMessage = Nothing
    }



-- JSON GRABBERS


{-| Create a jsonGrabber for a required field. Extracts the field from JSON,
or returns a MatchError if the field is absent. If the field is present but
the wrong type, returns an UnrecoverableValidationError.
-}
jsonFieldGrabber : String -> Json.Decode.Decoder a -> Cli.Decode.MatchErrorDetail -> JsonGrabber a
jsonFieldGrabber fieldName valueDecoder missingError blob =
    case Json.Decode.decodeValue (Json.Decode.field fieldName valueDecoder) blob of
        Ok value ->
            Ok ( [], value )

        Err decodeError ->
            case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.value) blob of
                Ok _ ->
                    Err
                        (Cli.Decode.UnrecoverableValidationError
                            { name = fieldName
                            , invalidReason = Json.Decode.errorToString decodeError
                            }
                        )

                Err _ ->
                    Err (Cli.Decode.MatchError missingError)


{-| Create a jsonGrabber for an optional field. Returns Nothing if absent.
-}
jsonOptionalFieldGrabber : String -> Json.Decode.Decoder a -> JsonGrabber (Maybe a)
jsonOptionalFieldGrabber fieldName valueDecoder blob =
    case Json.Decode.decodeValue (Json.Decode.field fieldName valueDecoder) blob of
        Ok value ->
            Ok ( [], Just value )

        Err decodeError ->
            case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.value) blob of
                Ok _ ->
                    Err
                        (Cli.Decode.UnrecoverableValidationError
                            { name = fieldName
                            , invalidReason = Json.Decode.errorToString decodeError
                            }
                        )

                Err _ ->
                    Ok ( [], Nothing )


{-| Create a jsonGrabber for an optional field with a default value.
-}
jsonOptionalFieldGrabberWithDefault : String -> Json.Decode.Decoder a -> a -> JsonGrabber a
jsonOptionalFieldGrabberWithDefault fieldName valueDecoder defaultValue blob =
    case Json.Decode.decodeValue (Json.Decode.field fieldName valueDecoder) blob of
        Ok value ->
            Ok ( [], value )

        Err _ ->
            Ok ( [], defaultValue )


{-| Create a jsonGrabber for a boolean flag. Defaults to False if absent.
-}
jsonFlagGrabber : String -> JsonGrabber Bool
jsonFlagGrabber fieldName blob =
    case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.bool) blob of
        Ok value ->
            Ok ( [], value )

        Err _ ->
            Ok ( [], False )



-- DATA GRABBERS


{-| Extract a required keyword arg value from parsed options.
-}
requiredKeywordArgGrabber : String -> DataGrabber String
requiredKeywordArgGrabber optionName { options } =
    case
        options
            |> List.Extra.find
                (\(Tokenizer.ParsedOption thisOptionName _) -> thisOptionName == optionName)
    of
        Nothing ->
            Cli.Decode.MatchError
                (Cli.Decode.MissingRequiredKeywordArg { name = optionName, customMessage = Nothing })
                |> Err

        Just (Tokenizer.ParsedOption _ (Tokenizer.KeywordArg optionArg)) ->
            Ok optionArg

        _ ->
            Cli.Decode.MatchError
                (Cli.Decode.KeywordArgMissingValue { name = optionName })
                |> Err


{-| Extract an optional keyword arg value from parsed options.
-}
optionalKeywordArgGrabber : String -> DataGrabber (Maybe String)
optionalKeywordArgGrabber optionName { options } =
    case
        options
            |> List.Extra.find
                (\(Tokenizer.ParsedOption thisOptionName _) -> thisOptionName == optionName)
    of
        Nothing ->
            Ok Nothing

        Just (Tokenizer.ParsedOption _ (Tokenizer.KeywordArg optionArg)) ->
            Ok (Just optionArg)

        _ ->
            Cli.Decode.MatchError
                (Cli.Decode.KeywordArgMissingValue { name = optionName })
                |> Err


{-| Collect all instances of a repeated keyword arg from parsed options.
-}
keywordArgListGrabber : String -> DataGrabber (List String)
keywordArgListGrabber flagName { options } =
    options
        |> List.filterMap
            (\(Tokenizer.ParsedOption optionName optionKind) ->
                case ( optionName == flagName, optionKind ) of
                    ( False, _ ) ->
                        Nothing

                    ( True, Tokenizer.KeywordArg optionValue ) ->
                        Just optionValue

                    ( True, _ ) ->
                        Nothing
            )
        |> Ok


{-| Extract a required positional arg by index.
-}
requiredPositionalArgGrabber : String -> DataGrabber String
requiredPositionalArgGrabber operandDescription { operands, operandsSoFar } =
    case
        operands
            |> List.Extra.getAt operandsSoFar
    of
        Just operandValue ->
            Ok operandValue

        Nothing ->
            Cli.Decode.MatchError
                (Cli.Decode.MissingRequiredPositionalArg
                    { name = operandDescription
                    , operandsSoFar = operandsSoFar
                    , customMessage = Nothing
                    }
                )
                |> Err


{-| Extract an optional positional arg by index.
-}
optionalPositionalArgGrabber : DataGrabber (Maybe String)
optionalPositionalArgGrabber flagsAndOperands =
    let
        operandsSoFar =
            UsageSpec.operandCount flagsAndOperands.usageSpecs - 1
    in
    flagsAndOperands.operands
        |> List.Extra.getAt operandsSoFar
        |> Ok


{-| Check if a flag is present in parsed options.
-}
flagGrabber : String -> DataGrabber Bool
flagGrabber flagName { options } =
    options
        |> List.member (Tokenizer.ParsedOption flagName Tokenizer.Flag)
        |> Ok


{-| Collect remaining positional args after the fixed ones.
-}
restArgsGrabber : DataGrabber (List String)
restArgsGrabber { operands, usageSpecs } =
    operands
        |> List.drop (UsageSpec.operandCount usageSpecs)
        |> Ok
