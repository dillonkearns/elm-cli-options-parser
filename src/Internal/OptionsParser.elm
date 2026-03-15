module Internal.OptionsParser exposing
    ( Decoder
    , OptionsParser(..)
    , OptionsParserRecord
    , getTsTypes
    , tryMatchJson
    )

import Cli.Decode
import Cli.Option.Internal as Internal
import Cli.OptionsParser.MatchResult
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Json.Decode
import Json.Encode as Encode
import Tokenizer exposing (ParsedOption)
import TsJson.Type


type OptionsParser cliOptions builderState
    = OptionsParser (OptionsParserRecord cliOptions)


type alias OptionsParserRecord cliOptions =
    { decoder : Decoder cliOptions
    , usageSpecs : List UsageSpec
    , description : Maybe String
    , subCommand : Maybe String
    , tsTypes : List ( String, TsJson.Type.Type )
    , jsonGrabber : Internal.JsonGrabber cliOptions
    }


type alias Decoder cliOptions =
    { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, cliOptions )


{-| Get the TsTypes collected from each option in this parser.
Returns a list of (name, tsType) pairs.
-}
getTsTypes : OptionsParser decodesTo builderState -> List ( String, TsJson.Type.Type )
getTsTypes (OptionsParser { tsTypes }) =
    tsTypes


{-| Try to match a JSON blob against this parser's jsonGrabber.
Normalizes the `$cli` object into flat fields before passing to jsonGrabber.
-}
tryMatchJson : Json.Decode.Value -> OptionsParser cliOptions builderState -> Cli.OptionsParser.MatchResult.MatchResult cliOptions
tryMatchJson blob (OptionsParser { jsonGrabber, usageSpecs, subCommand }) =
    let
        normalizedBlob : Json.Decode.Value
        normalizedBlob =
            normalizeCliJson usageSpecs blob

        baseMatchResult : Cli.OptionsParser.MatchResult.MatchResult cliOptions
        baseMatchResult =
            jsonGrabber normalizedBlob
                |> jsonGrabberResultToMatchResult

        structuralTypeValidationErrors : List Cli.Decode.ValidationError
        structuralTypeValidationErrors =
            subcommandJsonTypeValidationErrors subCommand blob
                ++ positionalJsonTypeValidationErrors usageSpecs blob baseMatchResult
    in
    case structuralTypeValidationErrors of
        firstValidationError :: otherValidationErrors ->
            Cli.OptionsParser.MatchResult.Match (Err (firstValidationError :: otherValidationErrors))

        [] ->
            let
                unexpectedShapeErrors : List Cli.OptionsParser.MatchResult.NoMatchReason
                unexpectedShapeErrors =
                    rawJsonShapeErrors subCommand usageSpecs blob

                positionalCountErrors : List Cli.OptionsParser.MatchResult.NoMatchReason
                positionalCountErrors =
                    extraJsonPositionalErrors usageSpecs blob baseMatchResult
            in
            case baseMatchResult of
                Cli.OptionsParser.MatchResult.Match _ ->
                    if List.isEmpty unexpectedShapeErrors && List.isEmpty positionalCountErrors then
                        baseMatchResult

                    else
                        Cli.OptionsParser.MatchResult.NoMatch (unexpectedShapeErrors ++ positionalCountErrors)

                Cli.OptionsParser.MatchResult.NoMatch reasons ->
                    Cli.OptionsParser.MatchResult.NoMatch
                        (unexpectedShapeErrors ++ positionalCountErrors ++ reasons)


jsonGrabberResultToMatchResult :
    Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, cliOptions )
    -> Cli.OptionsParser.MatchResult.MatchResult cliOptions
jsonGrabberResultToMatchResult jsonGrabberResult =
    case jsonGrabberResult of
        Err error ->
            case error of
                Cli.Decode.MatchError matchErrorDetail ->
                    Cli.OptionsParser.MatchResult.NoMatch
                        [ matchErrorDetailToNoMatchReason matchErrorDetail ]

                Cli.Decode.UnrecoverableValidationError validationError ->
                    Cli.OptionsParser.MatchResult.Match (Err [ validationError ])

                Cli.Decode.UnexpectedOptions unexpectedOptions ->
                    Cli.OptionsParser.MatchResult.NoMatch
                        (List.map Cli.OptionsParser.MatchResult.UnexpectedOption unexpectedOptions)

        Ok ( [], value ) ->
            Cli.OptionsParser.MatchResult.Match (Ok value)

        Ok ( validationErrors, _ ) ->
            Cli.OptionsParser.MatchResult.Match (Err validationErrors)


matchErrorDetailToNoMatchReason : Cli.Decode.MatchErrorDetail -> Cli.OptionsParser.MatchResult.NoMatchReason
matchErrorDetailToNoMatchReason detail =
    case detail of
        Cli.Decode.MissingExpectedFlag { name } ->
            Cli.OptionsParser.MatchResult.MissingExpectedFlag { name = name }

        Cli.Decode.MissingRequiredPositionalArg { name, customMessage } ->
            Cli.OptionsParser.MatchResult.MissingRequiredPositionalArg { name = name, customMessage = customMessage }

        Cli.Decode.MissingRequiredKeywordArg { name, customMessage } ->
            Cli.OptionsParser.MatchResult.MissingRequiredKeywordArg { name = name, customMessage = customMessage }

        Cli.Decode.KeywordArgMissingValue { name } ->
            Cli.OptionsParser.MatchResult.MissingRequiredKeywordArg { name = name, customMessage = Nothing }

        Cli.Decode.ExtraOperand ->
            Cli.OptionsParser.MatchResult.ExtraOperand

        Cli.Decode.MissingSubCommand { expectedSubCommand } ->
            Cli.OptionsParser.MatchResult.MissingSubCommand { expectedSubCommand = expectedSubCommand }

        Cli.Decode.WrongSubCommand { expectedSubCommand, actualSubCommand } ->
            Cli.OptionsParser.MatchResult.WrongSubCommand
                { expectedSubCommand = expectedSubCommand
                , actualSubCommand = actualSubCommand
                }


{-| Normalize a JSON blob with flat properties and `$cli` structural data into flat fields.
-}
normalizeCliJson : List UsageSpec -> Json.Decode.Value -> Json.Decode.Value
normalizeCliJson usageSpecs blob =
    let
        topLevelFields : List ( String, Json.Decode.Value )
        topLevelFields =
            case Json.Decode.decodeValue (Json.Decode.keyValuePairs Json.Decode.value) blob of
                Ok pairs ->
                    pairs |> List.filter (\( k, _ ) -> k /= "$cli")

                Err _ ->
                    []

        topLevelFieldNames : List String
        topLevelFieldNames =
            List.map Tuple.first topLevelFields

        maybeCli : Result Json.Decode.Error Json.Decode.Value
        maybeCli =
            Json.Decode.decodeValue (Json.Decode.field "$cli" Json.Decode.value) blob

        subcommandField : List ( String, Encode.Value )
        subcommandField =
            case maybeCli of
                Ok cliValue ->
                    case Json.Decode.decodeValue (Json.Decode.field "subcommand" Json.Decode.string) cliValue of
                        Ok subName ->
                            [ ( "subcommand", Encode.string subName ) ]

                        Err _ ->
                            []

                Err _ ->
                    []

        positionalFields : List ( String, Json.Decode.Value )
        positionalFields =
            case maybeCli of
                Ok cliValue ->
                    case Json.Decode.decodeValue (Json.Decode.field "positional" (Json.Decode.list Json.Decode.value)) cliValue of
                        Ok positionalValues ->
                            let
                                operandSpecs : List UsageSpec
                                operandSpecs =
                                    usageSpecs
                                        |> List.filter UsageSpec.isOperand

                                fixedFields : List ( String, Json.Decode.Value )
                                fixedFields =
                                    List.map2
                                        (\spec val -> ( UsageSpec.name spec, val ))
                                        operandSpecs
                                        positionalValues

                                restArgsName : Maybe String
                                restArgsName =
                                    usageSpecs
                                        |> List.filterMap
                                            (\spec ->
                                                case spec of
                                                    UsageSpec.RestArgs restName _ ->
                                                        Just restName

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.head

                                restFields : List ( String, Encode.Value )
                                restFields =
                                    case restArgsName of
                                        Just rName ->
                                            [ ( rName
                                              , Encode.list identity
                                                    (List.drop (List.length operandSpecs) positionalValues)
                                              )
                                            ]

                                        Nothing ->
                                            []
                            in
                            fixedFields ++ restFields

                        Err _ ->
                            []

                Err _ ->
                    []

        flagDefaults : List ( String, Encode.Value )
        flagDefaults =
            usageSpecs
                |> List.filterMap
                    (\spec ->
                        case spec of
                            UsageSpec.FlagOrKeywordArg (UsageSpec.Flag flagName) _ _ _ ->
                                if not (List.member flagName topLevelFieldNames) then
                                    Just ( flagName, Encode.bool False )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
    in
    Encode.object (topLevelFields ++ subcommandField ++ positionalFields ++ flagDefaults)


rawJsonShapeErrors : Maybe String -> List UsageSpec -> Json.Decode.Value -> List Cli.OptionsParser.MatchResult.NoMatchReason
rawJsonShapeErrors subCommand usageSpecs blob =
    let
        topLevelFields : List ( String, Json.Decode.Value )
        topLevelFields =
            jsonObjectFields blob

        cliValue : Maybe Json.Decode.Value
        cliValue =
            Json.Decode.decodeValue (Json.Decode.field "$cli" Json.Decode.value) blob
                |> Result.toMaybe

        unexpectedTopLevelFields : List Cli.OptionsParser.MatchResult.NoMatchReason
        unexpectedTopLevelFields =
            topLevelFields
                |> List.map Tuple.first
                |> List.filter (\fieldName -> not (List.member fieldName (allowedTopLevelFieldNames usageSpecs)))
                |> List.map Cli.OptionsParser.MatchResult.UnexpectedOption

        unexpectedCliFields : List Cli.OptionsParser.MatchResult.NoMatchReason
        unexpectedCliFields =
            case cliValue of
                Just actualCliValue ->
                    jsonObjectFields actualCliValue
                        |> List.map Tuple.first
                        |> List.filter (\fieldName -> not (List.member fieldName (allowedCliFieldNames subCommand usageSpecs)))
                        |> List.map (\fieldName -> Cli.OptionsParser.MatchResult.UnexpectedOption ("$cli." ++ fieldName))

                Nothing ->
                    []
    in
    unexpectedTopLevelFields ++ unexpectedCliFields


allowedTopLevelFieldNames : List UsageSpec -> List String
allowedTopLevelFieldNames usageSpecs =
    "$cli"
        :: (usageSpecs
                |> List.filterMap
                    (\usageSpec ->
                        case usageSpec of
                            UsageSpec.FlagOrKeywordArg _ _ _ _ ->
                                Just (UsageSpec.name usageSpec)

                            UsageSpec.Operand _ _ _ _ ->
                                Nothing

                            UsageSpec.RestArgs _ _ ->
                                Nothing
                    )
           )


allowedCliFieldNames : Maybe String -> List UsageSpec -> List String
allowedCliFieldNames subCommand usageSpecs =
    (case subCommand of
        Just _ ->
            [ "subcommand" ]

        Nothing ->
            []
    )
        ++ (if hasJsonPositionalInput usageSpecs then
                [ "positional" ]

            else
                []
           )


hasJsonPositionalInput : List UsageSpec -> Bool
hasJsonPositionalInput usageSpecs =
    usageSpecs
        |> List.any
            (\usageSpec ->
                case usageSpec of
                    UsageSpec.FlagOrKeywordArg _ _ _ _ ->
                        False

                    UsageSpec.Operand _ _ _ _ ->
                        True

                    UsageSpec.RestArgs _ _ ->
                        True
            )


extraJsonPositionalErrors :
    List UsageSpec
    -> Json.Decode.Value
    -> Cli.OptionsParser.MatchResult.MatchResult cliOptions
    -> List Cli.OptionsParser.MatchResult.NoMatchReason
extraJsonPositionalErrors usageSpecs blob baseMatchResult =
    if
        UsageSpec.hasRestArgs usageSpecs
            || not (hasJsonPositionalInput usageSpecs)
            || not (shouldValidateJsonPositionals baseMatchResult)
    then
        []

    else
        case Json.Decode.decodeValue (Json.Decode.field "$cli" (Json.Decode.field "positional" (Json.Decode.list Json.Decode.value))) blob of
            Ok positionalValues ->
                if List.length positionalValues > List.length (List.filter UsageSpec.isOperand usageSpecs) then
                    [ Cli.OptionsParser.MatchResult.ExtraOperand ]

                else
                    []

            Err _ ->
                []


positionalJsonTypeValidationErrors :
    List UsageSpec
    -> Json.Decode.Value
    -> Cli.OptionsParser.MatchResult.MatchResult cliOptions
    -> List Cli.Decode.ValidationError
positionalJsonTypeValidationErrors usageSpecs blob baseMatchResult =
    if hasJsonPositionalInput usageSpecs && shouldValidateJsonPositionals baseMatchResult then
        nestedJsonFieldTypeError
            { name = "$cli.positional"
            , decoder = Json.Decode.field "$cli" (Json.Decode.field "positional" (Json.Decode.list Json.Decode.value))
            , presenceDecoder = Json.Decode.field "$cli" (Json.Decode.field "positional" Json.Decode.value)
            , blob = blob
            }
            |> Maybe.map List.singleton
            |> Maybe.withDefault []

    else
        []


subcommandJsonTypeValidationErrors : Maybe String -> Json.Decode.Value -> List Cli.Decode.ValidationError
subcommandJsonTypeValidationErrors subCommand blob =
    case subCommand of
        Just _ ->
            nestedJsonFieldTypeError
                { name = "$cli.subcommand"
                , decoder = Json.Decode.field "$cli" (Json.Decode.field "subcommand" Json.Decode.string)
                , presenceDecoder = Json.Decode.field "$cli" (Json.Decode.field "subcommand" Json.Decode.value)
                , blob = blob
                }
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        Nothing ->
            []


shouldValidateJsonPositionals : Cli.OptionsParser.MatchResult.MatchResult cliOptions -> Bool
shouldValidateJsonPositionals baseMatchResult =
    case baseMatchResult of
        Cli.OptionsParser.MatchResult.Match _ ->
            True

        Cli.OptionsParser.MatchResult.NoMatch reasons ->
            not
                (List.any
                    (\reason ->
                        case reason of
                            Cli.OptionsParser.MatchResult.MissingExpectedFlag _ ->
                                True

                            Cli.OptionsParser.MatchResult.MissingSubCommand _ ->
                                True

                            Cli.OptionsParser.MatchResult.WrongSubCommand _ ->
                                True

                            _ ->
                                False
                    )
                    reasons
                )


nestedJsonFieldTypeError :
    { name : String
    , decoder : Json.Decode.Decoder a
    , presenceDecoder : Json.Decode.Decoder Json.Decode.Value
    , blob : Json.Decode.Value
    }
    -> Maybe Cli.Decode.ValidationError
nestedJsonFieldTypeError { name, decoder, presenceDecoder, blob } =
    case Json.Decode.decodeValue decoder blob of
        Ok _ ->
            Nothing

        Err decodeError ->
            case Json.Decode.decodeValue presenceDecoder blob of
                Ok _ ->
                    Just
                        { name = name
                        , invalidReason = Json.Decode.errorToString decodeError
                        }

                Err _ ->
                    Nothing


jsonObjectFields : Json.Decode.Value -> List ( String, Json.Decode.Value )
jsonObjectFields jsonValue =
    Json.Decode.decodeValue (Json.Decode.keyValuePairs Json.Decode.value) jsonValue
        |> Result.withDefault []
