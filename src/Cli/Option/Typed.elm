module Cli.Option.Typed exposing
    ( Option, CliDecoder
    , string, int, float, bool, fromDecoder
    , requiredKeywordArg, optionalKeywordArg, keywordArgList
    , requiredPositionalArg, optionalPositionalArg
    , flag, restArgs
    , oneOf, validateMap, validateMapIfPresent, withDefault
    , withDescription, withDisplayName
    )

{-| Typed option constructors for first-class JSON schema support.

Each constructor produces both a CLI parser and a JSON schema from a `CliDecoder`.
Use `string`, `int`, `float`, `bool` for primitives, or `fromDecoder` for custom types.


## Types

@docs Option, CliDecoder


## Decoders

@docs string, int, float, bool, fromDecoder


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
import Occurences exposing (Occurences(..))
import TsJson.Decode as TsDecode


{-| Re-exported from `Cli.Option` for convenience. See `Cli.Option.Option`.
-}
type alias Option from to builderState =
    Internal.Option from to builderState


{-| A decoder that knows how to parse values from both CLI args and JSON input.

Use `string`, `int`, `float`, `bool` for primitives, or `fromDecoder` for
custom types (objects, arrays, etc.) where the CLI input is a JSON string.

-}
type CliDecoder value
    = CliDecoder
        { cliParser : String -> String -> Result Cli.Decode.ProcessingError value
        , jsonDecoder : Json.Decode.Decoder value
        , tsDecoder : TsDecode.Decoder value
        }



-- Decoders


{-| A string value. In CLI mode, the raw string is passed through as-is.
In JSON mode, a JSON string field is decoded.

    Option.requiredKeywordArg "name" Option.string
    -- CLI: --name hello → "hello"
    -- JSON: {"name": "hello"} → "hello"

-}
string : CliDecoder String
string =
    CliDecoder
        { cliParser = \_ s -> Ok s
        , jsonDecoder = Json.Decode.string
        , tsDecoder = TsDecode.string
        }


{-| An integer value. In CLI mode, the string is parsed as a JSON integer.
In JSON mode, a JSON integer field is decoded.

    Option.requiredKeywordArg "count" Option.int
    -- CLI: --count 42 → 42
    -- JSON: {"count": 42} → 42

-}
int : CliDecoder Int
int =
    fromDecoder TsDecode.int


{-| A float value. In CLI mode, the string is parsed as a JSON number.
In JSON mode, a JSON number field is decoded.

    Option.requiredKeywordArg "rate" Option.float
    -- CLI: --rate 3.14 → 3.14
    -- JSON: {"rate": 3.14} → 3.14

-}
float : CliDecoder Float
float =
    fromDecoder TsDecode.float


{-| A boolean value. In CLI mode, the string is parsed as a JSON boolean.
In JSON mode, a JSON boolean field is decoded.

Note: for flags (present/absent), use `flag` instead.

    Option.requiredKeywordArg "dry-run" Option.bool
    -- CLI: --dry-run true → True
    -- JSON: {"dry-run": true} → True

-}
bool : CliDecoder Bool
bool =
    fromDecoder TsDecode.bool


{-| Create a `CliDecoder` from a `TsDecode.Decoder`. In CLI mode, the string
value is parsed as strict JSON. This means the CLI user must pass valid JSON.

For strings, this means the CLI value must be quoted: `--name '"hello"'`.
If you want bare string values, use `string` instead.

    import TsJson.Decode as TsDecode

    pointDecoder =
        TsDecode.succeed (\x y -> { x = x, y = y })
            |> TsDecode.andMap (TsDecode.field "x" TsDecode.int)
            |> TsDecode.andMap (TsDecode.field "y" TsDecode.int)

    Option.requiredKeywordArg "point" (Option.fromDecoder pointDecoder)
    -- CLI: --point '{"x":1,"y":2}'
    -- JSON: {"point": {"x": 1, "y": 2}}

-}
fromDecoder : TsDecode.Decoder value -> CliDecoder value
fromDecoder tsDecoder =
    CliDecoder
        { cliParser = decodeCliJson (TsDecode.decoder tsDecoder)
        , jsonDecoder = TsDecode.decoder tsDecoder
        , tsDecoder = tsDecoder
        }



-- Constructors


{-| A required keyword argument with a typed decoder.

    Option.requiredKeywordArg "count" Option.int
    -- CLI: --count 42 → 42
    -- JSON: {"count": 42} → 42
    -- Schema: {"type": "integer"}

-}
requiredKeywordArg : String -> CliDecoder value -> Option String value { position : BeginningOption, canAddMissingMessage : () }
requiredKeywordArg optionName (CliDecoder decoder) =
    Option
        { dataGrabber = Internal.requiredKeywordArgGrabber optionName
        , usageSpec = UsageSpec.keywordArg optionName Required
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (decoder.cliParser optionName)
        , meta = Internal.emptyMeta
        , tsType = TsDecode.tsType decoder.tsDecoder
        , jsonGrabber =
            Internal.jsonFieldGrabber optionName
                decoder.jsonDecoder
                (Cli.Decode.MissingRequiredKeywordArg { name = optionName, customMessage = Nothing })
        }


{-| An optional keyword argument with a typed decoder.

    Option.optionalKeywordArg "greeting" Option.string
    -- CLI: --greeting hi → Just "hi", omitted → Nothing
    -- JSON: {"greeting": "hi"} → Just "hi", absent → Nothing

-}
optionalKeywordArg : String -> CliDecoder value -> Option (Maybe String) (Maybe value) { position : BeginningOption }
optionalKeywordArg optionName (CliDecoder decoder) =
    Option
        { dataGrabber = Internal.optionalKeywordArgGrabber optionName
        , usageSpec = UsageSpec.keywordArg optionName Optional
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (\maybeString ->
                        case maybeString of
                            Just stringValue ->
                                decoder.cliParser optionName stringValue
                                    |> Result.map Just

                            Nothing ->
                                Ok Nothing
                    )
        , meta = Internal.emptyMeta
        , tsType = TsDecode.tsType decoder.tsDecoder
        , jsonGrabber = Internal.jsonOptionalFieldGrabber optionName decoder.jsonDecoder
        }


{-| A repeated keyword argument with a typed decoder for each value.

    Option.keywordArgList "header" Option.string
    -- CLI: --header "X-A: 1" --header "X-B: 2" → ["X-A: 1", "X-B: 2"]

-}
keywordArgList : String -> CliDecoder value -> Option (List String) (List value) { position : BeginningOption }
keywordArgList flagName (CliDecoder decoder) =
    Option
        { dataGrabber = Internal.keywordArgListGrabber flagName
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
                                            case decoder.cliParser flagName s of
                                                Ok v ->
                                                    Ok (v :: values)

                                                Err e ->
                                                    Err e
                                )
                                (Ok [])
                    )
        , meta = Internal.emptyMeta
        , tsType = TsDecode.tsType (TsDecode.list decoder.tsDecoder)
        , jsonGrabber = Internal.jsonOptionalFieldGrabberWithDefault flagName (Json.Decode.list decoder.jsonDecoder) []
        }


{-| A required positional argument with a typed decoder.

    Option.requiredPositionalArg "port" Option.int
    -- CLI: mytool 8080 → 8080
    -- JSON: {"port": 8080} → 8080

-}
requiredPositionalArg : String -> CliDecoder value -> Option String value { position : BeginningOption, canAddMissingMessage : () }
requiredPositionalArg operandDescription (CliDecoder decoder) =
    Option
        { dataGrabber = Internal.requiredPositionalArgGrabber operandDescription
        , usageSpec = UsageSpec.operand operandDescription
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (decoder.cliParser operandDescription)
        , meta = Internal.emptyMeta
        , tsType = TsDecode.tsType decoder.tsDecoder
        , jsonGrabber =
            Internal.jsonFieldGrabber operandDescription
                decoder.jsonDecoder
                (Cli.Decode.MissingRequiredPositionalArg
                    { name = operandDescription, operandsSoFar = 0, customMessage = Nothing }
                )
        }


{-| An optional positional argument with a typed decoder.
Must be used with `OptionsParser.withOptionalPositionalArg`.

    Option.optionalPositionalArg "revision" Option.string

-}
optionalPositionalArg : String -> CliDecoder value -> Option (Maybe String) (Maybe value) { position : OptionalPositionalArgOption }
optionalPositionalArg operandDescription (CliDecoder decoder) =
    Option
        { dataGrabber = Internal.optionalPositionalArgGrabber
        , usageSpec = UsageSpec.optionalPositionalArg operandDescription
        , decoder =
            Cli.Decode.decoder
                |> Cli.Decode.mapProcessingError
                    (\maybeString ->
                        case maybeString of
                            Just stringValue ->
                                decoder.cliParser operandDescription stringValue
                                    |> Result.map Just

                            Nothing ->
                                Ok Nothing
                    )
        , meta = Internal.emptyMeta
        , tsType = TsDecode.tsType decoder.tsDecoder
        , jsonGrabber = Internal.jsonOptionalFieldGrabber operandDescription decoder.jsonDecoder
        }


{-| A boolean flag. Always `Bool` — no decoder needed.

    Option.flag "verbose"
    -- CLI: --verbose → True, omitted → False
    -- JSON: {"verbose": true} → True, absent → False

-}
flag : String -> Option Bool Bool { position : BeginningOption }
flag flagName =
    Option
        { dataGrabber = Internal.flagGrabber flagName
        , usageSpec = UsageSpec.flag flagName Optional
        , decoder = Cli.Decode.decoder
        , meta = Internal.emptyMeta
        , tsType = TsDecode.tsType TsDecode.bool
        , jsonGrabber = Internal.jsonFlagGrabber flagName
        }


{-| Collect all remaining positional arguments. Must be used with `OptionsParser.withRestArgs`.

    Option.restArgs "files"
    -- CLI: mytool a.txt b.txt → ["a.txt", "b.txt"]

-}
restArgs : String -> Option (List String) (List String) { position : RestArgsOption }
restArgs restArgsDescription =
    Option
        { dataGrabber = Internal.restArgsGrabber
        , usageSpec = UsageSpec.restArgs restArgsDescription
        , decoder = Cli.Decode.decoder
        , meta = Internal.emptyMeta
        , tsType = TsDecode.tsType (TsDecode.list TsDecode.string)
        , jsonGrabber = Internal.jsonOptionalFieldGrabberWithDefault restArgsDescription (Json.Decode.list Json.Decode.string) []
        }



-- Re-exported modifiers


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


{-| Parse a CLI string as strict JSON. No fallback — the string must be valid JSON.
-}
decodeCliJson : Json.Decode.Decoder a -> String -> String -> Result Cli.Decode.ProcessingError a
decodeCliJson elmJsonDecoder optionName stringValue =
    case Json.Decode.decodeString elmJsonDecoder stringValue of
        Ok value ->
            Ok value

        Err err ->
            Err
                (Cli.Decode.UnrecoverableValidationError
                    { name = optionName
                    , invalidReason = Json.Decode.errorToString err
                    }
                )
