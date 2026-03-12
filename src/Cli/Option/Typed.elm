module Cli.Option.Typed exposing
    ( requiredKeywordArg, optionalKeywordArg, keywordArgList
    , requiredPositionalArg, optionalPositionalArg
    , flag, restArgs
    , oneOf, validateMap, validateMapIfPresent, withDefault
    , withDescription, withDisplayName
    )

{-| Typed option constructors that take a `TsDecode.Decoder` for first-class
JSON schema support. Each constructor produces both a CLI parser and a JSON
schema from the same decoder.

Use this module instead of `Cli.Option` when you want typed JSON schemas
(e.g., `"type": "integer"` instead of `"type": "string"` with manual validation).


## Keyword Arguments

@docs requiredKeywordArg, optionalKeywordArg, keywordArgList


## Positional Arguments

@docs requiredPositionalArg, optionalPositionalArg


## Flags and Rest Args

@docs flag, restArgs


## Modifiers

Re-exported from `Cli.Option` for convenience.

@docs oneOf, validateMap, validateMapIfPresent, withDefault
@docs withDescription, withDisplayName

-}

import Cli.Decode
import Cli.Option exposing (BeginningOption, OptionalPositionalArgOption, RestArgsOption)
import Cli.Option.Internal as Internal exposing (Option(..))
import Cli.UsageSpec as UsageSpec
import Json.Decode
import Json.Encode
import List.Extra
import Occurences exposing (Occurences(..))
import Tokenizer
import TsJson.Decode as TsDecode


{-| A required keyword argument with a typed decoder.

    Option.requiredKeywordArg "count" TsDecode.int
    -- CLI: --count 42 → 42
    -- JSON: {"count": 42} → 42
    -- Schema: {"type": "integer"}

-}
requiredKeywordArg : String -> TsDecode.Decoder value -> Option String value { position : BeginningOption, canAddMissingMessage : ()}
requiredKeywordArg optionName tsDecoder =
    let
        elmJsonDecoder =
            TsDecode.decoder tsDecoder
    in
    Option
        { dataGrabber =
            \{ options } ->
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
        , usageSpec = UsageSpec.keywordArg optionName Required
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (decodeCliString optionName elmJsonDecoder)
        , meta = { missingMessage = Nothing }
        , tsType = TsDecode.tsType tsDecoder
        , jsonGrabber =
            jsonFieldGrabber optionName elmJsonDecoder
                (Cli.Decode.MissingRequiredKeywordArg { name = optionName, customMessage = Nothing })
        }


{-| An optional keyword argument with a typed decoder.

    Option.optionalKeywordArg "greeting" TsDecode.string
    -- CLI: --greeting hi → Just "hi", omitted → Nothing
    -- JSON: {"greeting": "hi"} → Just "hi", absent → Nothing

-}
optionalKeywordArg : String -> TsDecode.Decoder value -> Option (Maybe String) (Maybe value) { position : BeginningOption}
optionalKeywordArg optionName tsDecoder =
    let
        elmJsonDecoder =
            TsDecode.decoder tsDecoder
    in
    Option
        { dataGrabber =
            \{ options } ->
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
        , usageSpec = UsageSpec.keywordArg optionName Optional
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (\maybeString ->
                        case maybeString of
                            Just stringValue ->
                                decodeCliString optionName elmJsonDecoder stringValue
                                    |> Result.map Just

                            Nothing ->
                                Ok Nothing
                    )
        , meta = { missingMessage = Nothing }
        , tsType = TsDecode.tsType tsDecoder
        , jsonGrabber =
            \blob ->
                case Json.Decode.decodeValue (Json.Decode.field optionName elmJsonDecoder) blob of
                    Ok value ->
                        Ok ( [], Just value )

                    Err decodeError ->
                        case Json.Decode.decodeValue (Json.Decode.field optionName Json.Decode.value) blob of
                            Ok _ ->
                                Err
                                    (Cli.Decode.UnrecoverableValidationError
                                        { name = optionName
                                        , invalidReason = Json.Decode.errorToString decodeError
                                        }
                                    )

                            Err _ ->
                                Ok ( [], Nothing )
        }


{-| A repeated keyword argument with a typed decoder for each value.

    Option.keywordArgList "header" TsDecode.string
    -- CLI: --header "X-A: 1" --header "X-B: 2" → ["X-A: 1", "X-B: 2"]

-}
keywordArgList : String -> TsDecode.Decoder value -> Option (List String) (List value) { position : BeginningOption}
keywordArgList flagName tsDecoder =
    let
        elmJsonDecoder =
            TsDecode.decoder tsDecoder
    in
    Option
        { dataGrabber =
            \{ options } ->
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
        , usageSpec = UsageSpec.keywordArg flagName ZeroOrMore
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (\strings ->
                        strings
                            |> List.foldr
                                (\s acc ->
                                    case acc of
                                        Err e ->
                                            Err e

                                        Ok values ->
                                            case decodeCliString flagName elmJsonDecoder s of
                                                Ok v ->
                                                    Ok (v :: values)

                                                Err e ->
                                                    Err e
                                )
                                (Ok [])
                    )
        , meta = { missingMessage = Nothing }
        , tsType = TsDecode.tsType (TsDecode.list tsDecoder)
        , jsonGrabber =
            \blob ->
                case Json.Decode.decodeValue (Json.Decode.field flagName (Json.Decode.list elmJsonDecoder)) blob of
                    Ok value ->
                        Ok ( [], value )

                    Err _ ->
                        Ok ( [], [] )
        }


{-| A required positional argument with a typed decoder.

    Option.requiredPositionalArg "port" TsDecode.int
    -- CLI: mytool 8080 → 8080
    -- JSON: {"port": 8080} → 8080

-}
requiredPositionalArg : String -> TsDecode.Decoder value -> Option String value { position : BeginningOption, canAddMissingMessage : ()}
requiredPositionalArg operandDescription tsDecoder =
    let
        elmJsonDecoder =
            TsDecode.decoder tsDecoder
    in
    Option
        { dataGrabber =
            \{ operands, operandsSoFar } ->
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
        , usageSpec = UsageSpec.operand operandDescription
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (decodeCliString operandDescription elmJsonDecoder)
        , meta = { missingMessage = Nothing }
        , tsType = TsDecode.tsType tsDecoder
        , jsonGrabber =
            jsonFieldGrabber operandDescription elmJsonDecoder
                (Cli.Decode.MissingRequiredPositionalArg
                    { name = operandDescription, operandsSoFar = 0, customMessage = Nothing }
                )
        }


{-| An optional positional argument with a typed decoder.
Must be used with `OptionsParser.withOptionalPositionalArg`.

    Option.optionalPositionalArg "revision" TsDecode.string

-}
optionalPositionalArg : String -> TsDecode.Decoder value -> Option (Maybe String) (Maybe value) { position : OptionalPositionalArgOption}
optionalPositionalArg operandDescription tsDecoder =
    let
        elmJsonDecoder =
            TsDecode.decoder tsDecoder
    in
    Option
        { dataGrabber =
            \flagsAndOperands ->
                let
                    operandsSoFar =
                        UsageSpec.operandCount flagsAndOperands.usageSpecs - 1

                    maybeArg =
                        flagsAndOperands.operands
                            |> List.Extra.getAt operandsSoFar
                in
                Ok maybeArg
        , usageSpec = UsageSpec.optionalPositionalArg operandDescription
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (\maybeString ->
                        case maybeString of
                            Just stringValue ->
                                decodeCliString operandDescription elmJsonDecoder stringValue
                                    |> Result.map Just

                            Nothing ->
                                Ok Nothing
                    )
        , meta = { missingMessage = Nothing }
        , tsType = TsDecode.tsType tsDecoder
        , jsonGrabber =
            \blob ->
                case Json.Decode.decodeValue (Json.Decode.field operandDescription elmJsonDecoder) blob of
                    Ok value ->
                        Ok ( [], Just value )

                    Err decodeError ->
                        case Json.Decode.decodeValue (Json.Decode.field operandDescription Json.Decode.value) blob of
                            Ok _ ->
                                Err
                                    (Cli.Decode.UnrecoverableValidationError
                                        { name = operandDescription
                                        , invalidReason = Json.Decode.errorToString decodeError
                                        }
                                    )

                            Err _ ->
                                Ok ( [], Nothing )
        }


{-| A boolean flag. Always `Bool` — no decoder needed.

    Option.flag "verbose"
    -- CLI: --verbose → True, omitted → False
    -- JSON: {"verbose": true} → True, absent → False

-}
flag : String -> Option Bool Bool { position : BeginningOption}
flag flagName =
    Option
        { dataGrabber =
            \{ options } ->
                if
                    options
                        |> List.member (Tokenizer.ParsedOption flagName Tokenizer.Flag)
                then
                    Ok True

                else
                    Ok False
        , usageSpec = UsageSpec.flag flagName Optional
        , decoder = Cli.Decode.decoder
        , meta = { missingMessage = Nothing }
        , tsType = TsDecode.tsType TsDecode.bool
        , jsonGrabber =
            \blob ->
                case Json.Decode.decodeValue (Json.Decode.field flagName Json.Decode.bool) blob of
                    Ok value ->
                        Ok ( [], value )

                    Err _ ->
                        Ok ( [], False )
        }


{-| Collect all remaining positional arguments. Must be used with `OptionsParser.withRestArgs`.

    Option.restArgs "files"
    -- CLI: mytool a.txt b.txt → ["a.txt", "b.txt"]

-}
restArgs : String -> Option (List String) (List String) { position : RestArgsOption}
restArgs restArgsDescription =
    Option
        { dataGrabber =
            \{ operands, usageSpecs } ->
                operands
                    |> List.drop (UsageSpec.operandCount usageSpecs)
                    |> Ok
        , usageSpec = UsageSpec.restArgs restArgsDescription
        , decoder = Cli.Decode.decoder
        , meta = { missingMessage = Nothing }
        , tsType = TsDecode.tsType (TsDecode.list TsDecode.string)
        , jsonGrabber =
            \blob ->
                case Json.Decode.decodeValue (Json.Decode.field restArgsDescription (Json.Decode.list Json.Decode.string)) blob of
                    Ok value ->
                        Ok ( [], value )

                    Err _ ->
                        Ok ( [], [] )
        }


{-| See `Cli.Option.oneOf`.
-}
oneOf : List ( String, value ) -> Option from String builderState -> Option from value builderState
oneOf =
    Cli.Option.oneOf


{-| See `Cli.Option.validateMap`.
-}
validateMap : (to -> Result String toMapped) -> Option from to builderState -> Option from toMapped builderState
validateMap =
    Cli.Option.validateMap


{-| See `Cli.Option.validateMapIfPresent`.
-}
validateMapIfPresent : (to -> Result String toMapped) -> Option (Maybe from) (Maybe to) builderState -> Option (Maybe from) (Maybe toMapped) builderState
validateMapIfPresent =
    Cli.Option.validateMapIfPresent


{-| See `Cli.Option.withDefault`.
-}
withDefault : to -> Option from (Maybe to) builderState -> Option from to builderState
withDefault =
    Cli.Option.withDefault


{-| See `Cli.Option.withDescription`.
-}
withDescription : String -> Option from to builderState -> Option from to builderState
withDescription =
    Cli.Option.withDescription


{-| See `Cli.Option.withDisplayName`.
-}
withDisplayName : String -> Option from to builderState -> Option from to builderState
withDisplayName =
    Cli.Option.withDisplayName



-- Internal helpers


decodeCliString : String -> Json.Decode.Decoder a -> String -> Result Cli.Decode.ProcessingError a
decodeCliString optionName elmJsonDecoder stringValue =
    case Json.Decode.decodeString elmJsonDecoder stringValue of
        Ok value ->
            Ok value

        Err _ ->
            case Json.Decode.decodeValue elmJsonDecoder (Json.Encode.string stringValue) of
                Ok value ->
                    Ok value

                Err wrappedErr ->
                    Err
                        (Cli.Decode.UnrecoverableValidationError
                            { name = optionName
                            , invalidReason = Json.Decode.errorToString wrappedErr
                            }
                        )


jsonFieldGrabber : String -> Json.Decode.Decoder a -> Cli.Decode.MatchErrorDetail -> Internal.JsonGrabber a
jsonFieldGrabber fieldName elmJsonDecoder missingError blob =
    case Json.Decode.decodeValue (Json.Decode.field fieldName elmJsonDecoder) blob of
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
