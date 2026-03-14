module Cli.Option.Typed exposing
    ( Option, CliDecoder
    , string, int, float, customDecoder
    , requiredPositionalArg
    , requiredKeywordArg, optionalKeywordArg, keywordArgList
    , flag
    , optionalPositionalArg, restArgs
    , oneOf
    , validate, validateIfPresent, validateMap, validateMapIfPresent
    , map, mapFlag, withDefault
    , BeginningOption, OptionalPositionalArgOption, RestArgsOption
    , withDescription, withDisplayName, withMissingMessage
    )

{-| Build a command-line options parser to validate and map a CLI command into a structured Elm type.

This is an alternative to [`Cli.Option`](Cli-Option) that is designed to
generate a JSON schema describing the valid ways to invoke the CLI command, but with more precise type information.
`Cli.Option` still generates a JSON schema, but [`Cli.Option.Typed.customDecoder`](#customDecoder) lets you pass in an
[`elm-ts-json` `Decoder`](https://package.elm-lang.org/packages/dillonkearns/elm-ts-json/latest/TsJson-Decode)
with arbitrary and fully typed JSON values, and the primitive `Option`s
like [`int`](#int) carry more precise type information instead of just `String`
in the JSON Schema output.

The vast majority of users will use `elm-cli-options-parser` through [`elm-pages`](https://elm-pages.com/)
when they build [`elm-pages` scripts](https://elm-pages.com/docs/elm-pages-scripts).
When you define an `elm-pages` script using [`Script.withSchema`](https://package.elm-lang.org/packages/dillonkearns/elm-pages/latest/Pages-Script#withCliOptions),
you pass in an [`elm-ts-json` `Encoder`](https://package.elm-lang.org/packages/dillonkearns/elm-ts-json/latest/TsJson-Encode)
and return a matching Elm type that the script will output as JSON,
and `elm-pages introspect` will automatically show all of the type information for your
CLI options and the output JSON as part of the introspection output.


## When to Use `Cli.Option.Typed`

All CLIs built with `elm-cli-options-parser` can be invoked either with traditional CLI arguments, or
with a single JSON string CLI argument, allowing for easier consumption by
LLM agents and programmatic invocation of your CLI. That, along with
the precise type information in the JSON Schema describing your CLI options, makes `Cli.Option.Typed`
a good choice for CLIs when they may be invoked programmatically or by LLM agents.

You can use [`Cli.Option`](Cli-Option) for a slightly simpler API that
treats all values as strings if automated tool access isn't a priority for your CLI.

Both modules produce the same `Option` type and work with the same
[`OptionsParser.with`](Cli-OptionsParser#with) pipeline, so they can be interwoven freely.


## Terminology

Here is the terminology used for building up Command-Line parsers with this library.

![Terminology Legend](https://raw.githubusercontent.com/dillonkearns/elm-cli-options-parser/master/terminology.png)

See the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src)
folder for end-to-end examples (including `TypedGreet.elm` which uses this module).


## Example

    import Cli.Option.Typed as Option
    import Cli.OptionsParser as OptionsParser exposing (with)
    import Cli.Program as Program

    type alias Options =
        { name : String
        , count : Int
        , verbose : Bool
        }

    programConfig : Program.Config Options
    programConfig =
        Program.config
            |> Program.add
                (OptionsParser.build Options
                    |> with
                        (Option.requiredKeywordArg
                            "name"
                            Option.string
                        )
                    |> with
                        (Option.requiredKeywordArg
                            "count"
                            Option.int
                        )
                    |> with
                        (Option.flag
                            "verbose"
                        )
                )

This parser handles both CLI and JSON input:

  - **CLI**: `mytool --name hello --count 3 --verbose`
  - **JSON**: `{ "name": "hello", "count": 3, "verbose": true, "$cli": {} }`

And `Program.toJsonSchema "mytool" programConfig` generates a JSON Schema with
proper types (`"type": "string"`, `"type": "integer"`, etc.).


## Types

@docs Option, CliDecoder


## Decoders

@docs string, int, float, customDecoder


## Positional Arguments

@docs requiredPositionalArg


## Keyword Arguments

@docs requiredKeywordArg, optionalKeywordArg, keywordArgList


## Flags

@docs flag


## Ending Options

These must be added with their corresponding `OptionsParser.with...` function,
not the regular `OptionsParser.with`. See the [`Cli.OptionsParser.BuilderState`](Cli-OptionsParser-BuilderState)
docs for why.

@docs optionalPositionalArg, restArgs


## Mutually Exclusive Values

@docs oneOf


## Validation

Validations allow you to guarantee that if you receive the data in Elm, it
meets a set of preconditions. If it doesn't, the user will see an error message
describing the validation error, which option it came from, and the value the
option had.

Note that failing a validation will not cause the next `OptionsParser` in
your `Cli.Program.Config` to be run. Instead,
if the OptionsParser is a match except for validation errors, you will get an
error message regardless.

Example:


    capitalizedNameRegex : String
    capitalizedNameRegex =
        "[A-Z][A-Za-z]*"

    validateParser : OptionsParser.OptionsParser ( String, Maybe Int ) BuilderState.NoMoreOptions
    validateParser =
        OptionsParser.build (\a b -> ( a, b ))
            |> OptionsParser.with
                (Option.requiredKeywordArg "name" Option.string
                    |> Option.validate (Cli.Validate.regex capitalizedNameRegex)
                )
            |> OptionsParser.with
                (Option.optionalKeywordArg "age" Option.int)

    {-
       $ ./validation --name Mozart --age 262
       Mozart is 262 years old

       $ ./validation --name mozart
       Validation errors:

       `name` failed a validation. Must be of form /[A-Z][A-Za-z]*/
       Value was:
       "mozart"
    -}

See [`Cli.Validate`](Cli-Validate) for some validation helpers that can be used
in conjunction with the following functions.

@docs validate, validateIfPresent, validateMap, validateMapIfPresent


## Mapping and Defaults

@docs map, mapFlag, withDefault


## Builder Position Types

@docs BeginningOption, OptionalPositionalArgOption, RestArgsOption


## Metadata

@docs withDescription, withDisplayName, withMissingMessage

-}

import Cli.Decode
import Cli.Option exposing (BeginningOption, OptionalPositionalArgOption, RestArgsOption)
import Cli.Option.Internal as Internal exposing (Option(..))
import Cli.UsageSpec as UsageSpec
import Cli.Validate
import Json.Decode
import Occurences exposing (Occurences(..))
import TsJson.Decode as TsDecode


{-| The type for an option in the pipeline. Use with
[`OptionsParser.with`](Cli-OptionsParser#with).
-}
type alias Option from to builderState =
    Internal.Option from to builderState


{-| Phantom type marker for options that can be used with
[`OptionsParser.with`](Cli-OptionsParser#with). Most option constructors
produce this type.
-}
type alias BeginningOption =
    Cli.Option.BeginningOption


{-| Phantom type marker for optional positional args. Must be used with
[`OptionsParser.withOptionalPositionalArg`](Cli-OptionsParser#withOptionalPositionalArg).
-}
type alias OptionalPositionalArgOption =
    Cli.Option.OptionalPositionalArgOption


{-| Phantom type marker for rest args. Must be used with
[`OptionsParser.withRestArgs`](Cli-OptionsParser#withRestArgs).
-}
type alias RestArgsOption =
    Cli.Option.RestArgsOption


{-| A decoder that knows how to parse values from both CLI args and JSON input.

Use `string`, `int`, `float` for primitives, or `customDecoder` for
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
    customDecoder TsDecode.int


{-| A float value. In CLI mode, the string is parsed as a JSON number.
In JSON mode, a JSON number field is decoded.

    Option.requiredKeywordArg "rate" Option.float
    -- CLI: --rate 3.14 → 3.14
    -- JSON: {"rate": 3.14} → 3.14

-}
float : CliDecoder Float
float =
    customDecoder TsDecode.float


{-| Create a `CliDecoder` from a [`TsDecode.Decoder`](https://package.elm-lang.org/packages/dillonkearns/elm-ts-json/latest/TsJson-Decode).
In CLI mode, the string value is parsed as a JSON value. This means the CLI user must pass valid JSON.

For strings, this means the CLI value must be quoted: `--name '"hello"'`.
If you want bare string values, use [`string`](#string) instead.

`customDecoder` is especially useful for decoding complex structured values like JSON objects or arrays.

    import TsJson.Decode as TsDecode

    pointDecoder : TsDecode.Decoder { x : Int, y : Int }
    pointDecoder =
        TsDecode.succeed (\x y -> { x = x, y = y })
            |> TsDecode.andMap (TsDecode.field "x" TsDecode.int)
            |> TsDecode.andMap (TsDecode.field "y" TsDecode.int)

    pointOption : Option String { x : Int, y : Int } { position : BeginningOption, canAddMissingMessage : () }
    pointOption =
        Option.requiredKeywordArg "point" (Option.customDecoder pointDecoder)

    -- CLI: --point '{"x":1,"y":2}'
    -- JSON: {"point": {"x": 1, "y": 2}}

-}
customDecoder : TsDecode.Decoder value -> CliDecoder value
customDecoder tsDecoder =
    CliDecoder
        { cliParser = decodeCliJson (TsDecode.decoder tsDecoder)
        , jsonDecoder = TsDecode.decoder tsDecoder
        , tsDecoder = tsDecoder
        }



-- Constructors


{-| A keyword argument that must be provided.

Example: `--name my-app` or `--name=my-app`

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


{-| A keyword argument that may be omitted.

Example: `--output main.js` or `--output=main.js`

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


{-| A keyword argument that can be provided multiple times.

Example: `--header "Auth: token" --header "Accept: json"`

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


{-| A positional argument that must be provided.

Example: `src/Main.elm` in `elm make src/Main.elm`

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


{-| An optional positional argument.

Must be used with [`OptionsParser.withOptionalPositionalArg`](Cli-OptionsParser#withOptionalPositionalArg)
(not `OptionsParser.with`).

Example: `<revision>` in `git log [<revision>]`
Parses to: `Just "abc123"` (or `Nothing` if omitted)

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


{-| A flag with no argument. Always `Bool` — no decoder needed.

Example: `--debug` in `elm make --debug`

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


{-| Collect all remaining positional arguments as a list.

Must be used with [`OptionsParser.withRestArgs`](Cli-OptionsParser#withRestArgs)
(not `OptionsParser.with`), and must be the last option in the pipeline.

Example: `<files>...` in `elm-test [<files>...]`
Parses to: `["tests/First.elm", "tests/Second.elm"]` (or `[]` if none provided)

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


{-| Mutually exclusive option values. Restricts the option to a fixed set of
string values, each mapped to an Elm value.

    type ReportFormat
        = Json
        | Junit
        | Console

    reportOption : Option String ReportFormat { position : BeginningOption, canAddMissingMessage : () }
    reportOption =
        Option.requiredKeywordArg "report" Option.string
            |> Option.oneOf
                [ ( "json", Json )
                , ( "junit", Junit )
                , ( "console", Console )
                ]

The help text will show the allowed values:

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

The JSON schema will include an `enum` constraint with the allowed values.

-}
oneOf : List ( String, value ) -> Option from String builderState -> Option from value builderState
oneOf =
    Cli.Option.oneOf


{-| Transform the option value, or fail with a validation error.

If the function returns `Err`, the error message is shown to the user.

    Option.requiredKeywordArg "count" Option.string
        |> Option.validateMap String.toInt

-}
validateMap : (to -> Result String toMapped) -> Option from to builderState -> Option from toMapped builderState
validateMap =
    Cli.Option.validateMap


{-| Like [`validateMap`](#validateMap), but only runs when the value is `Just`.
Does nothing for `Nothing`.

    Option.optionalKeywordArg "count" Option.string
        |> Option.validateMapIfPresent String.toInt

-}
validateMapIfPresent : (to -> Result String toMapped) -> Option (Maybe from) (Maybe to) builderState -> Option (Maybe from) (Maybe toMapped) builderState
validateMapIfPresent =
    Cli.Option.validateMapIfPresent


{-| Provide a default value for an optional option. Turns a `Maybe value`
into a plain `value`.

    Option.optionalKeywordArg "greeting" Option.string
        |> Option.withDefault "Hello"

-}
withDefault : to -> Option from (Maybe to) builderState -> Option from to builderState
withDefault =
    Cli.Option.withDefault


{-| Add a description shown in help text and JSON schema.

    Option.requiredKeywordArg "name" Option.string
        |> Option.withDescription "Your name for the greeting"

-}
withDescription : String -> Option from to builderState -> Option from to builderState
withDescription =
    Cli.Option.withDescription


{-| Set a custom display name (metavar) for a keyword argument's value placeholder
in help text and usage synopsis.

By default, the keyword arg name is uppercased (e.g., `--output-dir <OUTPUT_DIR>`).
Use this to provide a more descriptive placeholder.

    Option.requiredKeywordArg "output-dir" Option.string
        |> Option.withDisplayName "PATH"
    -- Shows as: --output-dir <PATH>

-}
withDisplayName : String -> Option from to builderState -> Option from to builderState
withDisplayName =
    Cli.Option.withDisplayName


{-| Add a custom error message for when a required option is missing.

This only works on required options (`requiredPositionalArg`, `requiredKeywordArg`).

    Option.requiredKeywordArg "repository" Option.string
        |> Option.withMissingMessage "You must specify a repository to clone."

-}
withMissingMessage : String -> Option from to { c | canAddMissingMessage : () } -> Option from to { c | canAddMissingMessage : () }
withMissingMessage =
    Cli.Option.withMissingMessage


{-| Transform an option's value. Use this for infallible transformations.
For transformations that can fail, use [`validateMap`](#validateMap) instead
so the user gets a helpful error message.

    Option.requiredKeywordArg "name" Option.string
        |> Option.map String.toUpper

    Option.requiredKeywordArg "output" Option.string
        |> Option.map (\path -> path ++ "/index.html")

-}
map : (toRaw -> toMapped) -> Option from toRaw builderState -> Option from toMapped builderState
map =
    Cli.Option.map


{-| Transform a flag's `Bool` into a custom type.

    type Verbosity
        = Quiet
        | Verbose

    verbosityOption : Option Bool Verbosity { position : BeginningOption }
    verbosityOption =
        Option.flag "verbose"
            |> Option.mapFlag { present = Verbose, absent = Quiet }

-}
mapFlag : { present : union, absent : union } -> Option from Bool builderState -> Option from union builderState
mapFlag =
    Cli.Option.mapFlag


{-| Run a validation on the parsed value. If validation fails, the user sees
the error message.

    Option.requiredKeywordArg "name" Option.string
        |> Option.validate
            (Cli.Validate.regex "^[A-Z][A-Za-z]*")

-}
validate : (to -> Cli.Validate.ValidationResult) -> Option from to builderState -> Option from to builderState
validate =
    Cli.Option.validate


{-| Like [`validate`](#validate), but only runs when the value is `Just`.
Does nothing for `Nothing`.
-}
validateIfPresent : (to -> Cli.Validate.ValidationResult) -> Option from (Maybe to) builderState -> Option from (Maybe to) builderState
validateIfPresent =
    Cli.Option.validateIfPresent



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
