module Cli.Option exposing
    ( requiredPositionalArg
    , optionalKeywordArg, requiredKeywordArg, keywordArgList
    , required, optional
    , flag
    , optionalPositionalArg, restArgs
    , oneOf
    , validate, validateIfPresent, validateMap, validateMapIfPresent
    , map, mapFlag, withDefault
    , withTypedJson, withTypedJsonIfPresent
    , withDescription, withDisplayName, withMissingMessage
    , Option, BeginningOption, OptionalPositionalArgOption, RestArgsOption
    , NoSchema, HasSchema
    )

{-| Here is the terminology used for building up Command-Line parsers with this library.

![Terminology Legend](https://raw.githubusercontent.com/dillonkearns/elm-cli-options-parser/master/terminology.png)

See the README and the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder for more in-depth examples of building
and using `Cli.Option`s.


## Positional Arguments

@docs requiredPositionalArg


## Keyword Arguments

@docs optionalKeywordArg, requiredKeywordArg, keywordArgList


## Typed Keyword Arguments

@docs required, optional


## Flags

@docs flag


## Ending Options

See note in `Cli.OptionsParser` docs.

@docs optionalPositionalArg, restArgs


## Transformations


### Mutually Exclusive Values

@docs oneOf


### Validation

Validations allow you to guarantee that if you receive the data in Elm, it
meets a set of preconditions. If it doesn't, the User will see an error message
describing the validation error, which option it came from, and the value the
option had.

Note that failing a validation will not cause the next `OptionsParser` in
your `Cli.Program.Config` to be run. Instead,
if the OptionsParser is a match except for validation errors, you will get an
error message regardless.

Example:


    capitalizedNameRegex =
        "[A-Z][A-Za-z]*"

    validateParser =
        OptionsParser.build (\a b -> ( a, b ))
            |> with
                (Option.requiredKeywordArg "name"
                    |> Option.validate (Cli.Validate.regex capitalizedNameRegex)
                )
            |> with
                (Option.optionalKeywordArg "age"
                    |> Option.validateMapIfPresent String.toInt
                )

    {-
       $ ./validation --name Mozart --age 262
       Mozart is 262 years old

       $ ./validation --name Mozart --age "Two-hundred and sixty-two"
       Validation errors:

       `age` failed a validation. could not convert string 'Two-hundred and sixty-two' to an Int
       Value was:
       Just "Two-hundred and sixty-two"
    -}

See `Cli.Validate` for some validation helpers that can be used in conjunction
with the following functions.

@docs validate, validateIfPresent, validateMap, validateMapIfPresent


### Mapping/Defaults

@docs map, mapFlag, withDefault


### Typed JSON

@docs withTypedJson, withTypedJsonIfPresent


### Metadata

@docs withDescription, withDisplayName, withMissingMessage


## Types

@docs Option, BeginningOption, OptionalPositionalArgOption, RestArgsOption
@docs NoSchema, HasSchema

-}

import Cli.Decode
import Cli.Option.Internal as Internal exposing (Option(..))
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Cli.Validate as Validate
import Json.Decode
import Json.Encode
import List.Extra
import Occurences exposing (Occurences(..))
import Tokenizer
import TsJson.Decode as TsDecode
import TsJson.Type


{-| Extract the TsType from a TsDecode.Decoder for use as the value-level schema type.
-}
tsTypeOf : TsDecode.Decoder a -> TsJson.Type.Type
tsTypeOf =
    TsDecode.tsType


{-| The type returned by the builder functions below. Use with `OptionsParser.with`.
-}
type alias Option from to middleOrEnding =
    Internal.Option from to middleOrEnding


{-| Phantom type marker for beginning options.

`BeginningOption`s can only be used with `OptionsParser.with`.

-}
type BeginningOption
    = BeginningOption Never


{-| Phantom type marker for rest args options.

`RestArgsOption`s can only be used with `OptionsParser.withRestArgs`.

-}
type RestArgsOption
    = RestArgsOption Never


{-| Phantom type marker for optional positional arg options.

`OptionalPositionalArgOption`s can only be used with `OptionsParser.withOptionalPositionalArg`.

-}
type OptionalPositionalArgOption
    = OptionalPositionalArgOption Never


{-| Phantom type marker indicating no JSON schema has been set on this option.
All option constructors start with `NoSchema`. Once `withTypedJson` is applied,
the marker changes to `HasSchema`, preventing it from being applied again.
-}
type NoSchema
    = NoSchema Never


{-| Phantom type marker indicating a JSON schema has been set on this option
via `withTypedJson`. This prevents `withTypedJson` from being applied a second time.
-}
type HasSchema
    = HasSchema Never


{-| Run a validation. (See an example in the Validation section above, or
in the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder).
-}
validate : (to -> Validate.ValidationResult) -> Option from to builderState -> Option from to builderState
validate validateFunction (Option option) =
    let
        optionName =
            UsageSpec.name option.usageSpec

        mappedDecoder : Cli.Decode.Decoder from to
        mappedDecoder =
            option.decoder
                |> Cli.Decode.mapValidationErrors
                    (\value ->
                        case validateFunction value of
                            Validate.Valid ->
                                Nothing

                            Validate.Invalid invalidReason ->
                                Just
                                    { name = optionName
                                    , invalidReason = invalidReason
                                    }
                    )

        mappedJsonGrabber : Internal.JsonGrabber to
        mappedJsonGrabber =
            \blob ->
                option.jsonGrabber blob
                    |> Result.map
                        (\( errors, value ) ->
                            case validateFunction value of
                                Validate.Valid ->
                                    ( errors, value )

                                Validate.Invalid invalidReason ->
                                    ( errors ++ [ { name = optionName, invalidReason = invalidReason } ], value )
                        )
    in
    Option
        { option
            | decoder = mappedDecoder
            , jsonGrabber = mappedJsonGrabber
        }


{-| Run a validation if the value is `Just someValue`. Or do nothing if the value is `Nothing`.
(See an example in the Validation section above, or in the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder).
-}
validateIfPresent : (to -> Validate.ValidationResult) -> Option from (Maybe to) builderState -> Option from (Maybe to) builderState
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


{-| A positional argument that must be provided.

Example: `src/Main.elm` in `elm make src/Main.elm`
Parses to: `"src/Main.elm"`

    Option.requiredPositionalArg "input"

-}
requiredPositionalArg : String -> Option String String { position : BeginningOption, canAddMissingMessage : (), hasSchema : NoSchema }
requiredPositionalArg operandDescription =
    buildRequiredOption
        (\{ operands, operandsSoFar } ->
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
        )
        (UsageSpec.operand operandDescription)
        (tsTypeOf TsDecode.string)
        (jsonFieldGrabber operandDescription Json.Decode.string
            (Cli.Decode.MissingRequiredPositionalArg
                { name = operandDescription, operandsSoFar = 0, customMessage = Nothing }
            )
        )


{-| A keyword argument that may be omitted.

Example: `--output main.js` or `--output=main.js`
Parses to: `Just "main.js"` (or `Nothing` if omitted)

    Option.optionalKeywordArg "output"

-}
optionalKeywordArg : String -> Option (Maybe String) (Maybe String) { position : BeginningOption, hasSchema : NoSchema }
optionalKeywordArg optionName =
    buildOptionalOption
        (\{ options } ->
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
        )
        (UsageSpec.keywordArg optionName Optional)
        (tsTypeOf TsDecode.string)
        (jsonOptionalFieldGrabber optionName Json.Decode.string)


{-| A keyword argument that must be provided.

Example: `--name my-app` or `--name=my-app`
Parses to: `"my-app"`

    Option.requiredKeywordArg "name"

-}
requiredKeywordArg : String -> Option String String { position : BeginningOption, canAddMissingMessage : (), hasSchema : NoSchema }
requiredKeywordArg optionName =
    buildRequiredOption
        (\{ options } ->
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
        )
        (UsageSpec.keywordArg optionName Required)
        (tsTypeOf TsDecode.string)
        (jsonFieldGrabber optionName Json.Decode.string
            (Cli.Decode.MissingRequiredKeywordArg { name = optionName, customMessage = Nothing })
        )


{-| A required keyword argument with a typed JSON decoder.

In CLI mode, the string value is parsed via `Json.Decode.decodeString`.
In JSON mode, the value is decoded natively from the JSON field.
The schema type comes from the decoder (e.g., `TsDecode.int` → `"type": "integer"`).

    Option.required "count" TsDecode.int
    -- CLI: --count 42 → string "42" → decodeString → Int
    -- JSON: {"count": 42} → Int directly
    -- Schema: {"type": "integer"}

-}
required : String -> TsDecode.Decoder value -> Option String value { position : BeginningOption, canAddMissingMessage : (), hasSchema : HasSchema }
required optionName tsDecoder =
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
        , meta = emptyMeta
        , tsType = tsTypeOf tsDecoder
        , jsonGrabber =
            jsonFieldGrabber optionName elmJsonDecoder
                (Cli.Decode.MissingRequiredKeywordArg { name = optionName, customMessage = Nothing })
        }


{-| An optional keyword argument with a typed JSON decoder.

In CLI mode, the string value is parsed via `Json.Decode.decodeString`.
In JSON mode, the value is decoded natively from the JSON field.
Returns `Nothing` if the option is omitted.

    Option.optional "count" TsDecode.int
    -- CLI: --count 42 → Just 42, omitted → Nothing
    -- JSON: {"count": 42} → Just 42, absent → Nothing
    -- Schema: {"type": "integer"} (not in required array)

-}
optional : String -> TsDecode.Decoder value -> Option (Maybe String) (Maybe value) { position : BeginningOption, hasSchema : HasSchema }
optional optionName tsDecoder =
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
        , meta = emptyMeta
        , tsType = tsTypeOf tsDecoder
        , jsonGrabber =
            \blob ->
                case Json.Decode.decodeValue (Json.Decode.field optionName elmJsonDecoder) blob of
                    Ok value ->
                        Ok ( [], Just value )

                    Err decodeError ->
                        -- Check if the field is absent (ok, return Nothing) or wrong type (error)
                        case Json.Decode.decodeValue (Json.Decode.field optionName Json.Decode.value) blob of
                            Ok _ ->
                                -- Field exists but wrong type
                                Err
                                    (Cli.Decode.UnrecoverableValidationError
                                        { name = optionName
                                        , invalidReason = Json.Decode.errorToString decodeError
                                        }
                                    )

                            Err _ ->
                                -- Field absent, that's fine for optional
                                Ok ( [], Nothing )
        }


{-| A flag with no argument.

Example: `--debug` in `elm make --debug`
Parses to: `True` (or `False` if omitted)

    Option.flag "debug"

-}
flag : String -> Option Bool Bool { position : BeginningOption, hasSchema : NoSchema }
flag flagName =
    buildOptionalOption
        (\{ options } ->
            if
                options
                    |> List.member (Tokenizer.ParsedOption flagName Tokenizer.Flag)
            then
                Ok True

            else
                Ok False
        )
        (UsageSpec.flag flagName Optional)
        (tsTypeOf TsDecode.bool)
        (jsonFlagGrabber flagName)


{-| Build an option for required arguments (has canAddMissingMessage capability).
-}
buildRequiredOption : Internal.DataGrabber a -> UsageSpec -> TsJson.Type.Type -> Internal.JsonGrabber a -> Option a a { position : BeginningOption, canAddMissingMessage : (), hasSchema : NoSchema }
buildRequiredOption dataGrabber usageSpec tsType jsonGrabber =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.decoder
        , meta = emptyMeta
        , tsType = tsType
        , jsonGrabber = jsonGrabber
        }


{-| Build an option for optional arguments (no canAddMissingMessage capability).
-}
buildOptionalOption : Internal.DataGrabber a -> UsageSpec -> TsJson.Type.Type -> Internal.JsonGrabber a -> Option a a { position : BeginningOption, hasSchema : NoSchema }
buildOptionalOption dataGrabber usageSpec tsType jsonGrabber =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.decoder
        , meta = emptyMeta
        , tsType = tsType
        , jsonGrabber = jsonGrabber
        }


{-| Build an ending option (like restArgs, optionalPositionalArg).
-}
buildEndingOption : Internal.DataGrabber a -> UsageSpec -> TsJson.Type.Type -> Internal.JsonGrabber a -> Option a a { position : position, hasSchema : NoSchema }
buildEndingOption dataGrabber usageSpec tsType jsonGrabber =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.decoder
        , meta = emptyMeta
        , tsType = tsType
        , jsonGrabber = jsonGrabber
        }


{-| Default empty metadata.
-}
emptyMeta : Internal.OptionMeta
emptyMeta =
    { missingMessage = Nothing
    }


{-| Decode a CLI string value using a JSON decoder.

Tries parsing as raw JSON first (handles numbers, bools, objects, arrays).
If that fails (e.g. "abc" is not valid JSON), falls back to treating the
string as a JSON string value (wraps it and re-decodes). This gives proper
type errors ("Expecting an INT") instead of "not valid JSON".

-}
decodeCliString : String -> Json.Decode.Decoder a -> String -> Result Cli.Decode.ProcessingError a
decodeCliString optionName elmJsonDecoder stringValue =
    case Json.Decode.decodeString elmJsonDecoder stringValue of
        Ok value ->
            Ok value

        Err directErr ->
            -- The string wasn't valid JSON (e.g. "abc"). Try treating it as a
            -- JSON string value so the decoder gives a proper type error.
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


{-| Create a jsonGrabber for a required field. Extracts the field from JSON,
or returns a MatchError if the field is absent. If the field is present but
the wrong type, returns an UnrecoverableValidationError.
-}
jsonFieldGrabber : String -> Json.Decode.Decoder a -> Cli.Decode.MatchErrorDetail -> Internal.JsonGrabber a
jsonFieldGrabber fieldName valueDecoder missingError =
    \blob ->
        case Json.Decode.decodeValue (Json.Decode.field fieldName valueDecoder) blob of
            Ok value ->
                Ok ( [], value )

            Err decodeError ->
                -- Distinguish between "field absent" and "field present, wrong type"
                case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.value) blob of
                    Ok _ ->
                        -- Field exists but wrong type
                        Err
                            (Cli.Decode.UnrecoverableValidationError
                                { name = fieldName
                                , invalidReason = Json.Decode.errorToString decodeError
                                }
                            )

                    Err _ ->
                        -- Field entirely absent
                        Err (Cli.Decode.MatchError missingError)


{-| Create a jsonGrabber for an optional field. Returns Nothing if absent.
-}
jsonOptionalFieldGrabber : String -> Json.Decode.Decoder a -> Internal.JsonGrabber (Maybe a)
jsonOptionalFieldGrabber fieldName valueDecoder =
    \blob ->
        case Json.Decode.decodeValue (Json.Decode.field fieldName valueDecoder) blob of
            Ok value ->
                Ok ( [], Just value )

            Err decodeError ->
                -- Check if the field is absent (ok, return Nothing) or wrong type (error)
                case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.value) blob of
                    Ok _ ->
                        -- Field exists but wrong type
                        Err
                            (Cli.Decode.UnrecoverableValidationError
                                { name = fieldName
                                , invalidReason = Json.Decode.errorToString decodeError
                                }
                            )

                    Err _ ->
                        -- Field absent, that's fine for optional
                        Ok ( [], Nothing )


{-| Create a jsonGrabber for an optional field with a default value.
-}
jsonOptionalFieldGrabberWithDefault : String -> Json.Decode.Decoder a -> a -> Internal.JsonGrabber a
jsonOptionalFieldGrabberWithDefault fieldName valueDecoder defaultValue =
    \blob ->
        case Json.Decode.decodeValue (Json.Decode.field fieldName valueDecoder) blob of
            Ok value ->
                Ok ( [], value )

            Err _ ->
                -- Field absent or wrong type — use default
                Ok ( [], defaultValue )


{-| Create a jsonGrabber for a boolean flag. Defaults to False if absent.
-}
jsonFlagGrabber : String -> Internal.JsonGrabber Bool
jsonFlagGrabber fieldName =
    \blob ->
        case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.bool) blob of
            Ok value ->
                Ok ( [], value )

            Err _ ->
                -- Flag absent or wrong type — default to False
                Ok ( [], False )


{-| Add a description to an option. This will be shown in help text.

    Option.requiredKeywordArg "name"
        |> Option.withDescription "Your name for the greeting"

-}
withDescription : String -> Option from to builderState -> Option from to builderState
withDescription description (Option option) =
    Option
        { option
            | usageSpec = UsageSpec.setDescription (Just description) option.usageSpec
        }


{-| Set a custom display name (metavar) for a keyword argument's value placeholder
in help text and usage synopsis.

By default, the keyword arg name is uppercased (e.g., `--output-dir <OUTPUT_DIR>`).
Use this to provide a more descriptive placeholder.

    Option.requiredKeywordArg "output-dir"
        |> Option.withDisplayName "PATH"
    -- Shows as: --output-dir <PATH>

-}
withDisplayName : String -> Option from to builderState -> Option from to builderState
withDisplayName displayName (Option option) =
    Option
        { option
            | usageSpec = UsageSpec.setDisplayName displayName option.usageSpec
        }


{-| Add a custom error message for when a required option is missing.

This only works on required options (requiredPositionalArg, requiredKeywordArg).

    Option.requiredPositionalArg "repository"
        |> Option.withMissingMessage "You must specify a repository to clone."

-}
withMissingMessage : String -> Option from to { c | canAddMissingMessage : () } -> Option from to { c | canAddMissingMessage : () }
withMissingMessage message (Option option) =
    Option
        { option
            | dataGrabber =
                \context ->
                    option.dataGrabber context
                        |> Result.mapError (addCustomMessageToError message)
            , jsonGrabber =
                \blob ->
                    option.jsonGrabber blob
                        |> Result.mapError (addCustomMessageToError message)
            , meta =
                { missingMessage = Just message
                }
        }


{-| Add custom message to an error if it's a missing required arg error.
-}
addCustomMessageToError : String -> Cli.Decode.ProcessingError -> Cli.Decode.ProcessingError
addCustomMessageToError message error =
    case error of
        Cli.Decode.MatchError detail ->
            Cli.Decode.MatchError (addCustomMessageToMatchError message detail)

        other ->
            other


{-| Add custom message to a MatchErrorDetail if applicable.
-}
addCustomMessageToMatchError : String -> Cli.Decode.MatchErrorDetail -> Cli.Decode.MatchErrorDetail
addCustomMessageToMatchError message detail =
    case detail of
        Cli.Decode.MissingRequiredPositionalArg record ->
            Cli.Decode.MissingRequiredPositionalArg { record | customMessage = Just message }

        Cli.Decode.MissingRequiredKeywordArg record ->
            Cli.Decode.MissingRequiredKeywordArg { record | customMessage = Just message }

        other ->
            other


{-| Transform an `Option`. For example, you may want to map an option from the
raw `String` that comes from the command line into a `Regex`, as in this code snippet.

    import Cli.Option as Option
    import Cli.OptionsParser as OptionsParser
    import Cli.Program as Program
    import Regex exposing (Regex)

    type alias CliOptions =
        { pattern : Regex }

    programConfig : Program.Config CliOptions
    programConfig =
        Program.config
            |> Program.add
                (OptionsParser.build buildCliOptions
                    |> OptionsParser.with
                        (Option.requiredPositionalArg "pattern"
                            |> Option.map Regex.regex
                        )
                )

-}
map : (toRaw -> toMapped) -> Option from toRaw builderState -> Option from toMapped builderState
map mapFn option =
    updateDecoder
        (\decoder -> Cli.Decode.map mapFn decoder)
        (\grabber -> \blob -> grabber blob |> Result.map (Tuple.mapSecond mapFn))
        option


updateDecoder : (Cli.Decode.Decoder from to -> Cli.Decode.Decoder from toNew) -> (Internal.JsonGrabber to -> Internal.JsonGrabber toNew) -> Option from to builderState -> Option from toNew builderState
updateDecoder mappedDecoder jsonGrabberMapper (Option { dataGrabber, usageSpec, decoder, meta, tsType, jsonGrabber }) =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = mappedDecoder decoder
        , meta = meta
        , tsType = tsType
        , jsonGrabber = jsonGrabberMapper jsonGrabber
        }


{-| Useful for using a custom union type for a flag instead of a `Bool`.

    import Cli.Option as Option
    import Cli.OptionsParser as OptionsParser
    import Cli.Program as Program

    type Verbosity
        = Quiet
        | Verbose

    type alias CliOptions =
        { verbosity : Verbosity
        }

    programConfig : Program.Config CliOptions
    programConfig =
        Program.config
            |> Program.add
                (OptionsParser.build CliOptions
                    |> OptionsParser.with
                        (Option.flag "verbose"
                            |> Option.mapFlag
                                { present = Verbose
                                , absent = Quiet
                                }
                        )
                )

-}
mapFlag : { present : union, absent : union } -> Option from Bool builderState -> Option from union builderState
mapFlag { present, absent } option =
    option
        |> map
            (\flagValue ->
                if flagValue then
                    present

                else
                    absent
            )


{-| Mutually exclusive option values.

    type ReportFormat
        = Json
        | Junit
        | Console

    type alias CliOptions =
        { reportFormat : ReportFormat
        , testFiles : List String
        }

    program : Program.Config CliOptions
    program =
        Program.config
            |> Program.add
                (OptionsParser.build CliOptions
                    |> with
                        (Option.optionalKeywordArg "report"
                            |> Option.withDefault "console"
                            |> Option.oneOf
                                [ "json" => Json
                                , "junit" => Junit
                                , "console" => Console
                                ]
                        )
                    |> OptionsParser.withRestArgs (Option.restArgs "TESTFILES")
                )

Now when you run it, you get the following in your help text:

```shell
$ ./elm-test --help
elm-test [--report <json|junit|console>] <TESTFILES>...
```

And if you run it with an unrecognized value, you get a validation error:

```shell
$ ./elm-test --report xml
Validation errors:

`report` failed a validation. Must be one of [json, junit, console]
Value was:
"xml"
```

-}
oneOf : List ( String, value ) -> Option from String builderState -> Option from value builderState
oneOf list (Option option) =
    validateMap
        (\argValue ->
            case
                list
                    |> List.Extra.findMap
                        (\( name, value ) ->
                            if name == argValue then
                                Just value

                            else
                                Nothing
                        )
            of
                Nothing ->
                    Err
                        ("Must be one of ["
                            ++ (list
                                    |> List.map (\( name, _ ) -> name)
                                    |> String.join ", "
                               )
                            ++ "]"
                        )

                Just matchingValue ->
                    Ok matchingValue
        )
        (Option
            { option
                | usageSpec =
                    UsageSpec.changeUsageSpec
                        (list
                            |> List.map (\( name, _ ) -> name)
                        )
                        option.usageSpec
                , tsType = tsTypeOf (TsDecode.stringUnion list)
            }
        )


{-| Transform the value through a map function. If it returns `Ok someValue` then
the `Option` will be transformed into `someValue`. If it returns `Err someError`
then the User of the Command-Line Interface will see `someError` with details
about the `Option` that had the validation error.

(See an example in the Validation section above, or
in the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder).

-}
validateMap : (to -> Result String toMapped) -> Option from to builderState -> Option from toMapped builderState
validateMap mapFn ((Option optionRecord) as option) =
    let
        optionName =
            UsageSpec.name optionRecord.usageSpec
    in
    updateDecoder
        (\decoder ->
            Cli.Decode.mapProcessingError
                (\value ->
                    case mapFn value of
                        Ok mappedValue ->
                            Ok mappedValue

                        Err invalidReason ->
                            Cli.Decode.UnrecoverableValidationError
                                { name = optionName
                                , invalidReason = invalidReason
                                }
                                |> Err
                )
                decoder
        )
        (\grabber ->
            \blob ->
                grabber blob
                    |> Result.andThen
                        (\( errors, value ) ->
                            case mapFn value of
                                Ok mappedValue ->
                                    Ok ( errors, mappedValue )

                                Err invalidReason ->
                                    Err
                                        (Cli.Decode.UnrecoverableValidationError
                                            { name = optionName
                                            , invalidReason = invalidReason
                                            }
                                        )
                        )
        )
        option


{-| Same as `validateMap` if the value is `Just someValue`. Does nothing if
the value is `Nothing`.

(See an example in the Validation section above, or
in the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder).

-}
validateMapIfPresent : (to -> Result String toMapped) -> Option (Maybe from) (Maybe to) builderState -> Option (Maybe from) (Maybe toMapped) builderState
validateMapIfPresent mapFn cliSpec =
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


{-| Provide a default value for the `Option`.
-}
withDefault : to -> Option from (Maybe to) builderState -> Option from to builderState
withDefault defaultValue option =
    updateDecoder
        (\decoder ->
            Cli.Decode.map
                (Maybe.withDefault defaultValue)
                decoder
        )
        (\grabber -> \blob -> grabber blob |> Result.map (Tuple.mapSecond (Maybe.withDefault defaultValue)))
        option


{-| Decode a string option's value as JSON using a `TsDecode.Decoder`.

This does three things:

1.  Parses the string value as JSON and decodes it using the provided decoder
2.  Attaches the decoder's JSON Schema to the option for introspection via `toJsonSchema`
3.  Sets the display name to `JSON` in help text (overridable with `withDisplayName`)

The `hasSchema : NoSchema` constraint ensures this can only be applied once per option.

    import TsJson.Codec as Codec

    todoCodec =
        Codec.object (\title desc -> { title = title, description = desc })
            |> Codec.field "title" .title Codec.string
            |> Codec.field "description" .description Codec.string
            |> Codec.buildObject

    Option.requiredKeywordArg "todo"
        |> Option.withTypedJson (Codec.decoder todoCodec)
        |> Option.withDescription "The todo item to create"

-}
withTypedJson :
    TsDecode.Decoder value
    -> Option from String { c | hasSchema : NoSchema }
    -> Option from value { c | hasSchema : HasSchema }
withTypedJson tsDecoder (Option optionRecord) =
    let
        optionName =
            UsageSpec.name optionRecord.usageSpec

        elmJsonDecoder =
            TsDecode.decoder tsDecoder
    in
    Option
        { dataGrabber = optionRecord.dataGrabber
        , meta = optionRecord.meta
        , tsType = TsDecode.tsType tsDecoder
        , usageSpec =
            optionRecord.usageSpec
                |> UsageSpec.setDisplayName "JSON"
        , decoder =
            -- CLI mode: extract string, decodeString
            Cli.Decode.mapProcessingError
                (\jsonString ->
                    case Json.Decode.decodeString elmJsonDecoder jsonString of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err
                                (Cli.Decode.UnrecoverableValidationError
                                    { name = optionName
                                    , invalidReason = Json.Decode.errorToString err
                                    }
                                )
                )
                optionRecord.decoder
        , jsonGrabber =
            -- JSON mode: decode directly from the JSON field
            \blob ->
                case Json.Decode.decodeValue (Json.Decode.field optionName elmJsonDecoder) blob of
                    Ok value ->
                        Ok ( [], value )

                    Err err ->
                        Err
                            (Cli.Decode.UnrecoverableValidationError
                                { name = optionName
                                , invalidReason = Json.Decode.errorToString err
                                }
                            )
        }


{-| Same as `withTypedJson`, but for optional (Maybe String) options.

    Option.optionalKeywordArg "filter"
        |> Option.withTypedJsonIfPresent (Codec.decoder filterCodec)

-}
withTypedJsonIfPresent :
    TsDecode.Decoder value
    -> Option (Maybe from) (Maybe String) { c | hasSchema : NoSchema }
    -> Option (Maybe from) (Maybe value) { c | hasSchema : HasSchema }
withTypedJsonIfPresent tsDecoder (Option optionRecord) =
    let
        optionName =
            UsageSpec.name optionRecord.usageSpec

        elmJsonDecoder =
            TsDecode.decoder tsDecoder
    in
    Option
        { dataGrabber = optionRecord.dataGrabber
        , meta = optionRecord.meta
        , tsType = TsDecode.tsType tsDecoder
        , usageSpec =
            optionRecord.usageSpec
                |> UsageSpec.setDisplayName "JSON"
        , decoder =
            -- CLI mode: optionally extract string, decodeString
            Cli.Decode.mapProcessingError
                (\maybeString ->
                    case maybeString of
                        Just jsonString ->
                            case Json.Decode.decodeString elmJsonDecoder jsonString of
                                Ok value ->
                                    Ok (Just value)

                                Err err ->
                                    Err
                                        (Cli.Decode.UnrecoverableValidationError
                                            { name = optionName
                                            , invalidReason = Json.Decode.errorToString err
                                            }
                                        )

                        Nothing ->
                            Ok Nothing
                )
                optionRecord.decoder
        , jsonGrabber =
            -- JSON mode: optionally decode from the JSON field
            \blob ->
                case Json.Decode.decodeValue (Json.Decode.field optionName elmJsonDecoder) blob of
                    Ok value ->
                        Ok ( [], Just value )

                    Err _ ->
                        Ok ( [], Nothing )
        }


{-| A keyword argument that can be provided multiple times.

Example: `--header "Auth: token" --header "Accept: json"`
Parses to: `["Auth: token", "Accept: json"]`

    Option.keywordArgList "header"

-}
keywordArgList : String -> Option (List String) (List String) { position : BeginningOption, hasSchema : NoSchema }
keywordArgList flagName =
    buildOptionalOption
        (\{ options } ->
            options
                |> List.filterMap
                    (\(Tokenizer.ParsedOption optionName optionKind) ->
                        case ( optionName == flagName, optionKind ) of
                            ( False, _ ) ->
                                Nothing

                            ( True, Tokenizer.KeywordArg optionValue ) ->
                                Just optionValue

                            ( True, _ ) ->
                                -- TODO this should probably be an error
                                Nothing
                    )
                |> Ok
        )
        (UsageSpec.keywordArg flagName ZeroOrMore)
        (tsTypeOf (TsDecode.list TsDecode.string))
        (jsonOptionalFieldGrabberWithDefault flagName (Json.Decode.list Json.Decode.string) [])


{-| Note that this must be used with `OptionsParser.withOptionalPositionalArg`.
-}
optionalPositionalArg : String -> Option (Maybe String) (Maybe String) { position : OptionalPositionalArgOption, hasSchema : NoSchema }
optionalPositionalArg operandDescription =
    buildEndingOption
        (\flagsAndOperands ->
            let
                operandsSoFar : Int
                operandsSoFar =
                    UsageSpec.operandCount flagsAndOperands.usageSpecs
                        - 1

                maybeArg : Maybe String
                maybeArg =
                    flagsAndOperands.operands
                        |> List.Extra.getAt operandsSoFar
            in
            Ok maybeArg
        )
        (UsageSpec.optionalPositionalArg operandDescription)
        (tsTypeOf TsDecode.string)
        (jsonOptionalFieldGrabber operandDescription Json.Decode.string)


{-| Note that this must be used with `OptionsParser.withRestArgs`.
-}
restArgs : String -> Option (List String) (List String) { position : RestArgsOption, hasSchema : NoSchema }
restArgs restArgsDescription =
    buildEndingOption
        (\{ operands, usageSpecs } ->
            operands
                |> List.drop (UsageSpec.operandCount usageSpecs)
                |> Ok
        )
        (UsageSpec.restArgs restArgsDescription)
        (tsTypeOf (TsDecode.list TsDecode.string))
        (jsonOptionalFieldGrabberWithDefault restArgsDescription (Json.Decode.list Json.Decode.string) [])
