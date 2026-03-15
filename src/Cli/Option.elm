module Cli.Option exposing
    ( requiredPositionalArg
    , optionalKeywordArg, requiredKeywordArg, keywordArgList
    , flag
    , optionalPositionalArg, restArgs
    , oneOf
    , validate, validateIfPresent, validateMap, validateMapIfPresent
    , map, mapFlag, withDefault
    , withDescription, withDisplayName, withMissingMessage
    , Option, BeginningOption, OptionalPositionalArgOption, RestArgsOption
    )

{-| Build command-line options as string values, with validation and transformation.

This module treats all CLI input as strings. Use [`validateMap`](#validateMap) to parse
strings into typed values, [`oneOf`](#oneOf) for enumerated values, and
[`validate`](#validate) for custom validation.

For typed options with JSON schema generation, see [`Cli.Option.Typed`](Cli-Option-Typed).

Here is the terminology used for building up Command-Line parsers with this library.

![Terminology Legend](https://raw.githubusercontent.com/dillonkearns/elm-cli-options-parser/master/terminology.png)

See the README and the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder for more in-depth examples of building
and using `Cli.Option`s.


## Positional Arguments

@docs requiredPositionalArg


## Keyword Arguments

@docs optionalKeywordArg, requiredKeywordArg, keywordArgList


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


### Metadata

@docs withDescription, withDisplayName, withMissingMessage


## Types

@docs Option, BeginningOption, OptionalPositionalArgOption, RestArgsOption

-}

import Cli.Decode
import Cli.Option.Internal as Internal exposing (Option(..))
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Cli.Validate as Validate
import Json.Decode
import List.Extra
import Occurences exposing (Occurences(..))
import TsJson.Decode as TsDecode
import TsJson.Type


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


{-| Run a validation. (See an example in the Validation section above, or
in the [`examples`](https://github.com/dillonkearns/elm-cli-options-parser/tree/master/examples/src) folder).
-}
validate : (to -> Validate.ValidationResult) -> Option from to builderState -> Option from to builderState
validate validateFunction (Option option) =
    let
        optionName : String
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
requiredPositionalArg : String -> Option String String { position : BeginningOption, canAddMissingMessage : () }
requiredPositionalArg operandDescription =
    buildRequiredOption
        (Internal.requiredPositionalArgGrabber operandDescription)
        (UsageSpec.operand operandDescription)
        (TsDecode.tsType TsDecode.string)
        (Internal.jsonFieldGrabber operandDescription
            Json.Decode.string
            (Cli.Decode.MissingRequiredPositionalArg
                { name = operandDescription, operandsSoFar = 0, customMessage = Nothing }
            )
        )


{-| A keyword argument that may be omitted.

Example: `--output main.js` or `--output=main.js`
Parses to: `Just "main.js"` (or `Nothing` if omitted)

    Option.optionalKeywordArg "output"

-}
optionalKeywordArg : String -> Option (Maybe String) (Maybe String) { position : BeginningOption }
optionalKeywordArg optionName =
    buildOptionalOption
        (Internal.optionalKeywordArgGrabber optionName)
        (UsageSpec.keywordArg optionName Optional)
        (TsDecode.tsType TsDecode.string)
        (Internal.jsonOptionalFieldGrabber optionName Json.Decode.string)


{-| A keyword argument that must be provided.

Example: `--name my-app` or `--name=my-app`
Parses to: `"my-app"`

    Option.requiredKeywordArg "name"

-}
requiredKeywordArg : String -> Option String String { position : BeginningOption, canAddMissingMessage : () }
requiredKeywordArg optionName =
    buildRequiredOption
        (Internal.requiredKeywordArgGrabber optionName)
        (UsageSpec.keywordArg optionName Required)
        (TsDecode.tsType TsDecode.string)
        (Internal.jsonFieldGrabber optionName
            Json.Decode.string
            (Cli.Decode.MissingRequiredKeywordArg { name = optionName, customMessage = Nothing })
        )


{-| A flag with no argument.

Example: `--debug` in `elm make --debug`
Parses to: `True` (or `False` if omitted)

    Option.flag "debug"

-}
flag : String -> Option Bool Bool { position : BeginningOption }
flag flagName =
    buildOptionalOption
        (Internal.flagGrabber flagName)
        (UsageSpec.flag flagName Optional)
        (TsDecode.tsType TsDecode.bool)
        (Internal.jsonFlagGrabber flagName)


{-| Build an option for required arguments (has canAddMissingMessage capability).
-}
buildRequiredOption : Internal.DataGrabber a -> UsageSpec -> TsJson.Type.Type -> Internal.JsonGrabber a -> Option a a { position : BeginningOption, canAddMissingMessage : () }
buildRequiredOption dataGrabber usageSpec tsType jsonGrabber =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.decoder
        , meta = Internal.emptyMeta
        , tsType = tsType
        , jsonGrabber = jsonGrabber
        }


{-| Build an option for optional arguments (no canAddMissingMessage capability).
-}
buildOptionalOption : Internal.DataGrabber a -> UsageSpec -> TsJson.Type.Type -> Internal.JsonGrabber a -> Option a a { position : BeginningOption }
buildOptionalOption dataGrabber usageSpec tsType jsonGrabber =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.decoder
        , meta = Internal.emptyMeta
        , tsType = tsType
        , jsonGrabber = jsonGrabber
        }


{-| Build an ending option (like restArgs, optionalPositionalArg).
-}
buildEndingOption : Internal.DataGrabber a -> UsageSpec -> TsJson.Type.Type -> Internal.JsonGrabber a -> Option a a { position : position }
buildEndingOption dataGrabber usageSpec tsType jsonGrabber =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.decoder
        , meta = Internal.emptyMeta
        , tsType = tsType
        , jsonGrabber = jsonGrabber
        }


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


{-| Transform an option's value. Use this for infallible transformations.
For transformations that can fail, use [`validateMap`](#validateMap) instead
so the user gets a helpful error message.

    Option.requiredKeywordArg "name"
        |> Option.map String.toUpper

    Option.requiredKeywordArg "output"
        |> Option.map (\path -> path ++ "/index.html")

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
                , tsType = TsDecode.tsType (TsDecode.stringUnion list)
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
        optionName : String
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


{-| Provide a default value for an optional `Option`. Turns a `Maybe value`
into a plain `value`.

    Option.optionalKeywordArg "greeting"
        |> Option.withDefault "Hello"

If `--greeting` is omitted, the option's value will be `"Hello"` instead of `Nothing`.

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


{-| A keyword argument that can be provided multiple times.

Example: `--header "Auth: token" --header "Accept: json"`
Parses to: `["Auth: token", "Accept: json"]`

    Option.keywordArgList "header"

-}
keywordArgList : String -> Option (List String) (List String) { position : BeginningOption }
keywordArgList flagName =
    buildOptionalOption
        (Internal.keywordArgListGrabber flagName)
        (UsageSpec.keywordArg flagName ZeroOrMore)
        (TsDecode.tsType (TsDecode.list TsDecode.string))
        (Internal.jsonOptionalFieldGrabberWithDefault flagName (Json.Decode.list Json.Decode.string) [])


{-| An optional positional argument.

Must be used with [`OptionsParser.withOptionalPositionalArg`](Cli-OptionsParser#withOptionalPositionalArg)
(not `OptionsParser.with`).

Example: `<revision>` in `git log [<revision>]`
Parses to: `Just "abc123"` (or `Nothing` if omitted)

    Option.optionalPositionalArg "revision"

-}
optionalPositionalArg : String -> Option (Maybe String) (Maybe String) { position : OptionalPositionalArgOption }
optionalPositionalArg operandDescription =
    buildEndingOption
        Internal.optionalPositionalArgGrabber
        (UsageSpec.optionalPositionalArg operandDescription)
        (TsDecode.tsType TsDecode.string)
        (Internal.jsonOptionalFieldGrabber operandDescription Json.Decode.string)


{-| Collect all remaining positional arguments as a list.

Must be used with [`OptionsParser.withRestArgs`](Cli-OptionsParser#withRestArgs)
(not `OptionsParser.with`), and must be the last option in the pipeline.

Example: `<files>...` in `elm-test [<files>...]`
Parses to: `["tests/First.elm", "tests/Second.elm"]` (or `[]` if none provided)

    Option.restArgs "files"

-}
restArgs : String -> Option (List String) (List String) { position : RestArgsOption }
restArgs restArgsDescription =
    buildEndingOption
        Internal.restArgsGrabber
        (UsageSpec.restArgs restArgsDescription)
        (TsDecode.tsType (TsDecode.list TsDecode.string))
        (Internal.jsonOptionalFieldGrabberWithDefault restArgsDescription (Json.Decode.list Json.Decode.string) [])
