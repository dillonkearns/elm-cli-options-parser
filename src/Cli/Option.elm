module Cli.Option
    exposing
        ( EndingOption
        , MiddleOption
        , Option(Option)
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

@docs Option, MiddleOption, EndingOption


## Positional Arguments

@docs positionalArg


## Keyword Arguments

@docs optionalKeywordArg, requiredKeywordArg, keywordArgList


## Flags

@docs flag


## Ending Options

@docs optionalPositionalArg, restArgs


## Mutually Exclusive Values

@docs oneOf


## Validation

@docs validate, validateIfPresent, validateMap, validateMapIfPresent


## Mapping/Defaults

@docs map, mapFlag, withDefault

-}

import Cli.Decode
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Cli.Validate as Validate
import List.Extra
import Occurences exposing (Occurences(Optional, Required, ZeroOrMore))
import Tokenizer


{-| TODO
-}
type Option from to middleOrEnding
    = Option (InnerOption from to)


{-| `MiddleOption`s can only be used with `Command.with`.

`EndingOption`s can only be used with `Command.endWith`.

-}
type MiddleOption
    = MiddleOption


{-| `MiddleOption`s can only be used with `Command.with`.

`EndingOption`s can only be used with `Command.endWith`.

-}
type EndingOption
    = EndingOption


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


{-| TODO
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


{-| TODO
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


{-| TODO
-}
positionalArg : String -> Option String String MiddleOption
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


{-| TODO
-}
optionalKeywordArg : String -> Option (Maybe String) (Maybe String) MiddleOption
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


{-| TODO
-}
requiredKeywordArg : String -> Option String String MiddleOption
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


{-| TODO
-}
flag : String -> Option Bool Bool MiddleOption
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


{-| TODO
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


{-| TODO
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


{-| TODO
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


{-| TODO
-}
keywordArgList : String -> Option (List String) (List String) MiddleOption
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


{-| TODO
-}
optionalPositionalArg : String -> Option (Maybe String) (Maybe String) EndingOption
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


{-| TODO
-}
restArgs : String -> Option (List String) (List String) EndingOption
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
