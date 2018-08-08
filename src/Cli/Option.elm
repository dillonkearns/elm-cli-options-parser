module Cli.Option
    exposing
        ( BeginningOption
        , Option(Option)
        , OptionalPositionalArgOption
        , RestArgsOption
        , flag
        , keywordArgList
        , map
        , mapFlag
        , oneOf
        , optionalKeywordArg
        , optionalPositionalArg
        , positionalArg
        , requiredKeywordArg
        , restArgs
        , validate
        , validateIfPresent
        , validateMap
        , validateMapIfPresent
        , withDefault
        )

{-|


## Positional Arguments

@docs positionalArg


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
        OptionsParser.build (,)
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


## Types

@docs Option, BeginningOption, OptionalPositionalArgOption, RestArgsOption

-}

import Cli.Decode
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Cli.Validate as Validate
import List.Extra
import Occurences exposing (Occurences(Optional, Required, ZeroOrMore))
import Tokenizer


{-| -}
type Option from to middleOrEnding
    = Option (InnerOption from to)


{-| `BeginningOption`s can only be used with `OptionsParser.with`.

`OptionalPositionalArgOption`s can only be used with `OptionsParser.withOptionalPositionalArg`.

-}
type BeginningOption
    = BeginningOption


{-| `RestArgsOption`s can only be used with `OptionsParser.withRestArgs`.
-}
type RestArgsOption
    = RestArgsOption


{-| `BeginningOption`s can only be used with `OptionsParser.with`.

`OptionalPositionalArgOption`s can only be used with `OptionsParser.withOptionalPositionalArg`.

-}
type OptionalPositionalArgOption
    = OptionalPositionalArgOption


type alias InnerOption from to =
    { dataGrabber : DataGrabber from
    , usageSpec : UsageSpec
    , decoder : Cli.Decode.Decoder from to
    }


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec
    , operands : List String
    , options : List Tokenizer.ParsedOption
    , operandsSoFar : Int
    }
    -> Result Cli.Decode.ProcessingError decodesTo


{-| Run a validation.
-}
validate : (to -> Validate.ValidationResult) -> Option from to anything -> Option from to anything
validate validateFunction (Option option) =
    let
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
                                    { name = UsageSpec.name option.usageSpec
                                    , invalidReason = invalidReason
                                    , valueAsString = toString value
                                    }
                    )
    in
    Option
        { option
            | decoder = mappedDecoder
        }


{-| Run a validation if the value is `Just someValue`. Or do nothing if the value is `Nothing`.
-}
validateIfPresent : (to -> Validate.ValidationResult) -> Option from (Maybe to) anything -> Option from (Maybe to) anything
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


{-| -}
positionalArg : String -> Option String String BeginningOption
positionalArg operandDescription =
    buildOption
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


{-| -}
optionalKeywordArg : String -> Option (Maybe String) (Maybe String) BeginningOption
optionalKeywordArg optionName =
    buildOption
        (\{ operands, options } ->
            case
                options
                    |> List.Extra.find
                        (\(Tokenizer.ParsedOption thisOptionName optionKind) -> thisOptionName == optionName)
            of
                Nothing ->
                    Ok Nothing

                Just (Tokenizer.ParsedOption _ (Tokenizer.KeywordArg optionArg)) ->
                    Ok (Just optionArg)

                _ ->
                    Cli.Decode.MatchError ("Expected option " ++ optionName ++ " to have arg but found none.") |> Err
        )
        (UsageSpec.keywordArg optionName Optional)


{-| -}
requiredKeywordArg : String -> Option String String BeginningOption
requiredKeywordArg optionName =
    buildOption
        (\{ operands, options } ->
            case
                options
                    |> List.Extra.find
                        (\(Tokenizer.ParsedOption thisOptionName optionKind) -> thisOptionName == optionName)
            of
                Nothing ->
                    Cli.Decode.MatchError ("Expected to find option " ++ optionName ++ " but only found options " ++ toString options) |> Err

                Just (Tokenizer.ParsedOption _ (Tokenizer.KeywordArg optionArg)) ->
                    Ok optionArg

                _ ->
                    Cli.Decode.MatchError ("Expected option " ++ optionName ++ " to have arg but found none.") |> Err
        )
        (UsageSpec.keywordArg optionName Required)


{-| -}
flag : String -> Option Bool Bool BeginningOption
flag flagName =
    buildOption
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


buildOption : DataGrabber a -> UsageSpec -> Option a a anything
buildOption dataGrabber usageSpec =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.decoder
        }


{-| TODO
-}
map : (toRaw -> toMapped) -> Option from toRaw anything -> Option from toMapped anything
map mapFn (Option ({ dataGrabber, usageSpec, decoder } as option)) =
    Option { option | decoder = Cli.Decode.map mapFn decoder }


{-| TODO
-}
mapFlag : { present : union, absent : union } -> Option from Bool anything -> Option from union anything
mapFlag { present, absent } option =
    option
        |> map
            (\flag ->
                if flag then
                    present
                else
                    absent
            )


type alias MutuallyExclusiveValue union =
    ( String, union )


{-| TODO
-}
oneOf : value -> List (MutuallyExclusiveValue value) -> Option from String anything -> Option from value anything
oneOf default list (Option option) =
    validateMap
        (\argValue ->
            case
                list
                    |> List.Extra.find (\( name, value ) -> name == argValue)
                    |> Maybe.map (\( name, value ) -> value)
            of
                Nothing ->
                    Err
                        ("Must be one of ["
                            ++ (list
                                    |> List.map (\( name, value ) -> name)
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
                            |> List.map (\( name, value ) -> name)
                        )
                        option.usageSpec
            }
        )


{-| Transform the value through a map function. If it returns `Ok someValue` then
the `Option` will be transformed into `someValue`. If it returns `Err someError`
then the User of the Command-Line Interface will see `someError` with details
about the `Option` that had the validation error.
-}
validateMap : (to -> Result String toMapped) -> Option from to anything -> Option from toMapped anything
validateMap mapFn (Option option) =
    let
        mappedDecoder =
            Cli.Decode.mapProcessingError
                (\value ->
                    case mapFn value of
                        Ok mappedValue ->
                            Ok mappedValue

                        Err invalidReason ->
                            Cli.Decode.UnrecoverableValidationError
                                { name = UsageSpec.name option.usageSpec
                                , invalidReason = invalidReason
                                , valueAsString = toString value
                                }
                                |> Err
                )
                option.decoder
    in
    Option
        { option
            | decoder = mappedDecoder
        }


{-| Same as `validateMap` if the value is `Just someValue`. Does nothing if
the value is `Nothing`.
-}
validateMapIfPresent : (to -> Result String toMapped) -> Option (Maybe from) (Maybe to) anything -> Option (Maybe from) (Maybe toMapped) anything
validateMapIfPresent mapFn ((Option { dataGrabber, usageSpec, decoder }) as cliSpec) =
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
withDefault : to -> Option from (Maybe to) anything -> Option from to anything
withDefault defaultValue (Option option) =
    Option
        { option
            | decoder =
                Cli.Decode.map
                    (Maybe.withDefault defaultValue)
                    option.decoder
        }


{-| -}
keywordArgList : String -> Option (List String) (List String) BeginningOption
keywordArgList flagName =
    buildOption
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


{-| Note that this must be used with `OptionsParser.withOptionalPositionalArg`.
-}
optionalPositionalArg : String -> Option (Maybe String) (Maybe String) OptionalPositionalArgOption
optionalPositionalArg operandDescription =
    buildOption
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


{-| Note that this must be used with `OptionsParser.withRestArgs`.
-}
restArgs : String -> Option (List String) (List String) RestArgsOption
restArgs restArgsDescription =
    buildOption
        (\({ operands, usageSpecs } as stuff) ->
            let
                restArgs =
                    operands
                        |> List.drop (UsageSpec.operandCount usageSpecs)
            in
            Ok restArgs
        )
        (UsageSpec.restArgs restArgsDescription)
