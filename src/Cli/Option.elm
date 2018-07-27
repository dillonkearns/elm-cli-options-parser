module Cli.Option
    exposing
        ( Option(Option)
        , flag
        , keywordArgList
        , map
        , mapFlag
        , oneOf
        , optionalKeywordArg
        , positionalArg
        , requiredKeywordArg
        , validate
        , validateIfPresent
        , validateMap
        , validateMapIfPresent
        , withDefault
        )

{-|

@docs Option


## Positional Arguments

@docs positionalArg


## Keyword Arguments

@docs optionalKeywordArg, requiredKeywordArg, keywordArgList


## Flags

@docs flag


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
type Option from to
    = Option (InnerOption from to)


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
validate : (to -> Validate.ValidationResult) -> Option from to -> Option from to
validate validateFunction (Option { dataGrabber, usageSpec, decoder }) =
    let
        mappedDecoder : Cli.Decode.Decoder from to
        mappedDecoder =
            decoder
                |> Cli.Decode.mapValidationErrors
                    (\value ->
                        case validateFunction value of
                            Validate.Valid ->
                                Nothing

                            Validate.Invalid invalidReason ->
                                Just
                                    { name = UsageSpec.name usageSpec
                                    , invalidReason = invalidReason
                                    , valueAsString = toString value
                                    }
                    )
    in
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = mappedDecoder
        }


{-| TODO
-}
validateIfPresent : (to -> Validate.ValidationResult) -> Option from (Maybe to) -> Option from (Maybe to)
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
positionalArg : String -> Option String String
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
optionalKeywordArg : String -> Option (Maybe String) (Maybe String)
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
requiredKeywordArg : String -> Option String String
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
flag : String -> Option Bool Bool
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


buildOption : DataGrabber anything -> UsageSpec -> Option anything anything
buildOption dataGrabber usageSpec =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.decoder
        }


{-| TODO
-}
map : (toRaw -> toMapped) -> Option from toRaw -> Option from toMapped
map mapFn (Option ({ dataGrabber, usageSpec, decoder } as innerOption)) =
    Option { innerOption | decoder = Cli.Decode.map mapFn decoder }


{-| TODO
-}
mapFlag : { present : union, absent : union } -> Option from Bool -> Option from union
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
oneOf : value -> List (MutuallyExclusiveValue value) -> Option from String -> Option from value
oneOf default list (Option { dataGrabber, usageSpec, decoder }) =
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
            { dataGrabber = dataGrabber
            , usageSpec =
                UsageSpec.changeUsageSpec
                    (list
                        |> List.map (\( name, value ) -> name)
                    )
                    usageSpec
            , decoder = decoder
            }
        )


{-| TODO
-}
validateMap : (to -> Result String toMapped) -> Option from to -> Option from toMapped
validateMap mapFn (Option { dataGrabber, usageSpec, decoder }) =
    let
        mappedDecoder =
            Cli.Decode.mapProcessingError
                (\value ->
                    case mapFn value of
                        Ok mappedValue ->
                            Ok mappedValue

                        Err invalidReason ->
                            Cli.Decode.UnrecoverableValidationError
                                { name = UsageSpec.name usageSpec
                                , invalidReason = invalidReason
                                , valueAsString = toString value
                                }
                                |> Err
                )
                decoder
    in
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = mappedDecoder
        }


{-| TODO
-}
validateMapIfPresent : (to -> Result String toMapped) -> Option (Maybe from) (Maybe to) -> Option (Maybe from) (Maybe toMapped)
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
withDefault : to -> Option from (Maybe to) -> Option from to
withDefault defaultValue (Option { dataGrabber, usageSpec, decoder }) =
    Option
        { dataGrabber = dataGrabber
        , usageSpec = usageSpec
        , decoder = Cli.Decode.map (Maybe.withDefault defaultValue) decoder
        }


{-| TODO
-}
keywordArgList : String -> Option (List String) (List String)
keywordArgList flagName =
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
                                    -- TODO this should probably be an error
                                    Nothing
                        )
                    |> Ok
        , usageSpec = UsageSpec.keywordArg flagName ZeroOrMore
        , decoder = Cli.Decode.decoder
        }
