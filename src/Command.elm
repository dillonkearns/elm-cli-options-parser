module Command exposing (Command, CommandBuilder, ValidationResult(..), build, buildWithDoc, captureRestOperands, expectFlag, flag, getUsageSpecs, hardcoded, keywordArgList, mapNew, optionalKeywordArg, positionalArg, requiredKeywordArg, synopsis, toCommand, tryMatch, validate, validateIfPresent, with, withDefault)

import Cli.Decode
import Cli.Expect exposing (Expectation)
import Cli.Unit exposing (CliUnit(..))
import Cli.UsageSpec exposing (..)
import List.Extra
import Occurences exposing (Occurences(..))
import Parser exposing (ParsedOption)


getUsageSpecs : Command decodesTo -> List UsageSpec
getUsageSpecs (Command { usageSpecs }) =
    usageSpecs


synopsis : String -> Command decodesTo -> String
synopsis programName command =
    command
        |> (\(Command record) -> record)
        |> Cli.UsageSpec.synopsis programName


tryMatch : List String -> Command msg -> Maybe (Result (List Cli.Decode.ValidationError) msg)
tryMatch argv ((Command { decoder, usageSpecs }) as command) =
    let
        decoder =
            command
                |> expectedOperandCountOrFail
                |> failIfUnexpectedOptionsNew
                |> getDecoder
    in
    decoder
        (Parser.flagsAndOperands usageSpecs argv
            |> (\record ->
                    { options = record.options
                    , operands = record.operands
                    , usageSpecs = usageSpecs
                    }
               )
        )
        |> (\result ->
                case result of
                    Err error ->
                        Nothing

                    Ok ( [], value ) ->
                        Just (Ok value)

                    Ok ( validationErrors, value ) ->
                        Just (Err validationErrors)
           )


hasRestArgs : List UsageSpec -> Bool
hasRestArgs usageSpecs =
    List.any
        (\usageSpec ->
            case usageSpec of
                RestArgs _ ->
                    True

                _ ->
                    False
        )
        usageSpecs


expectedOperandCountOrFail : Command msg -> Command msg
expectedOperandCountOrFail ((Command ({ decoder, usageSpecs } as command)) as fullCommand) =
    Command
        { command
            | decoder =
                \({ operands } as stuff) ->
                    if
                        not (hasRestArgs usageSpecs)
                            && (operands |> List.length)
                            > (usageSpecs
                                |> List.filterMap
                                    (\option ->
                                        case option of
                                            Operand operand ->
                                                Just operand

                                            Option _ _ ->
                                                Nothing

                                            RestArgs _ ->
                                                Nothing
                                    )
                                |> List.length
                              )
                    then
                        Err "Wrong number of operands"
                    else
                        decoder stuff
        }


getDecoder :
    Command msg
    ->
        { operands : List String
        , options : List ParsedOption
        , usageSpecs : List UsageSpec
        }
    -> Result String ( List Cli.Decode.ValidationError, msg )
getDecoder (Command { decoder }) =
    decoder


failIfUnexpectedOptionsNew : Command msg -> Command msg
failIfUnexpectedOptionsNew ((Command ({ decoder, usageSpecs } as command)) as fullCommand) =
    Command
        { command
            | decoder =
                \({ options } as stuff) ->
                    let
                        unexpectedOptions =
                            List.filterMap
                                (\(Parser.ParsedOption optionName optionKind) ->
                                    if optionExistsNew usageSpecs optionName == Nothing then
                                        Just optionName
                                    else
                                        Nothing
                                )
                                options
                    in
                    if List.isEmpty unexpectedOptions then
                        decoder stuff
                    else
                        Err ("Unexpected options " ++ toString unexpectedOptions)
        }


optionExistsNew : List UsageSpec -> String -> Maybe Option
optionExistsNew usageSpecs thisOptionName =
    usageSpecs
        |> List.filterMap
            (\usageSpec ->
                case usageSpec of
                    Option option occurences ->
                        option
                            |> Just

                    Operand _ ->
                        Nothing

                    RestArgs _ ->
                        Nothing
            )
        |> List.Extra.find (\option -> optionName option == thisOptionName)


type CommandBuilder msg
    = CommandBuilder
        { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result String ( List Cli.Decode.ValidationError, msg )
        , usageSpecs : List UsageSpec
        , description : Maybe String
        }


toCommand : CommandBuilder msg -> Command msg
toCommand (CommandBuilder record) =
    Command record


captureRestOperands : String -> CommandBuilder (List String -> msg) -> Command msg
captureRestOperands restOperandsDescription (CommandBuilder ({ usageSpecs, description, decoder } as record)) =
    Command
        { usageSpecs = usageSpecs ++ [ RestArgs restOperandsDescription ]
        , description = description
        , decoder =
            \({ operands } as stuff) ->
                let
                    restOperands =
                        operands
                            |> List.drop (operandCount usageSpecs)
                in
                resultMap (\fn -> fn restOperands) (decoder stuff)
        }


type Command msg
    = Command
        { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result String ( List Cli.Decode.ValidationError, msg )
        , usageSpecs : List UsageSpec
        , description : Maybe String
        }


build : msg -> CommandBuilder msg
build msgConstructor =
    CommandBuilder
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        }


buildWithDoc : msg -> String -> CommandBuilder msg
buildWithDoc msgConstructor docString =
    CommandBuilder
        { usageSpecs = []
        , description = Just docString
        , decoder = \_ -> Ok ( [], msgConstructor )
        }


hardcoded : value -> CommandBuilder (value -> msg) -> CommandBuilder msg
hardcoded hardcodedValue (CommandBuilder ({ decoder } as command)) =
    CommandBuilder
        { command
            | decoder =
                \stuff -> resultMap (\fn -> fn hardcodedValue) (decoder stuff)
        }


resultMap : (a -> value) -> Result String ( List Cli.Decode.ValidationError, a ) -> Result String ( List Cli.Decode.ValidationError, value )
resultMap mapFunction result =
    result
        |> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value ))


expectFlag : String -> CommandBuilder msg -> CommandBuilder msg
expectFlag flagName (CommandBuilder ({ usageSpecs, decoder } as command)) =
    let
        formattedFlag =
            "--" ++ flagName
    in
    CommandBuilder
        { command
            | usageSpecs = usageSpecs ++ [ Option (Flag flagName) Required ]
            , decoder =
                \({ options } as stuff) ->
                    if
                        options
                            |> List.member (Parser.ParsedOption flagName Parser.Flag)
                    then
                        decoder stuff
                    else
                        ("Expect flag " ++ formattedFlag)
                            |> Err
        }


operandCount : List UsageSpec -> Int
operandCount usageSpecs =
    usageSpecs
        |> List.filterMap
            (\spec ->
                case spec of
                    Option _ _ ->
                        Nothing

                    Operand operandName ->
                        Just operandName

                    RestArgs _ ->
                        Nothing
            )
        |> List.length


keywordArgList : String -> CliUnit (List String) (List String)
keywordArgList flagName =
    CliUnit
        (\{ options } ->
            options
                |> List.filterMap
                    (\(Parser.ParsedOption optionName optionKind) ->
                        case ( optionName == flagName, optionKind ) of
                            ( False, _ ) ->
                                Nothing

                            ( True, Parser.OptionWithArg optionValue ) ->
                                Just optionValue

                            ( True, _ ) ->
                                -- TODO this should probably be an error
                                Nothing
                    )
                |> Ok
        )
        (Option (OptionWithStringArg flagName) ZeroOrMore)
        Cli.Decode.decoder


positionalArg : String -> CliUnit String String
positionalArg operandDescription =
    CliUnit
        (\{ usageSpecs, operands, operandsSoFar } ->
            case
                operands
                    |> List.Extra.getAt operandsSoFar
            of
                Just operandValue ->
                    Ok operandValue

                Nothing ->
                    Err ("Expect operand " ++ operandDescription ++ "at " ++ toString operandsSoFar ++ " but had operands " ++ toString operands)
        )
        (Operand operandDescription)
        Cli.Decode.decoder


withDefault : value -> CliUnit (Maybe value) (Maybe value) -> CliUnit (Maybe value) value
withDefault defaultValue (CliUnit dataGrabber usageSpec decoder) =
    CliUnit dataGrabber usageSpec (decoder |> Cli.Decode.map (Maybe.withDefault defaultValue))


optionalKeywordArg : String -> CliUnit (Maybe String) (Maybe String)
optionalKeywordArg optionName =
    CliUnit
        (\{ operands, options } ->
            case
                options
                    |> List.Extra.find
                        (\(Parser.ParsedOption thisOptionName optionKind) -> thisOptionName == optionName)
            of
                Nothing ->
                    Ok Nothing

                Just (Parser.ParsedOption _ (Parser.OptionWithArg optionArg)) ->
                    Ok (Just optionArg)

                _ ->
                    Err ("Expected option " ++ optionName ++ " to have arg but found none.")
        )
        (Option (OptionWithStringArg optionName) Optional)
        Cli.Decode.decoder


requiredKeywordArg : String -> CliUnit String String
requiredKeywordArg optionName =
    CliUnit
        (\{ operands, options } ->
            case
                options
                    |> List.Extra.find
                        (\(Parser.ParsedOption thisOptionName optionKind) -> thisOptionName == optionName)
            of
                Nothing ->
                    Err ("Expected to find option " ++ optionName ++ " but only found options " ++ toString options)

                Just (Parser.ParsedOption _ (Parser.OptionWithArg optionArg)) ->
                    Ok optionArg

                _ ->
                    Err ("Expected option " ++ optionName ++ " to have arg but found none.")
        )
        (Option (OptionWithStringArg optionName) Required)
        Cli.Decode.decoder


flag : String -> CliUnit Bool Bool
flag flagName =
    CliUnit
        (\{ options } ->
            if
                options
                    |> List.member (Parser.ParsedOption flagName Parser.Flag)
            then
                Ok True
            else
                Ok False
        )
        (Option (Flag flagName) Optional)
        Cli.Decode.decoder


mapNew : (toRaw -> toMapped) -> CliUnit from toRaw -> CliUnit from toMapped
mapNew mapFn (CliUnit dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) =
    CliUnit dataGrabber usageSpec (Cli.Decode.map mapFn decoder)


type ValidationResult
    = Valid
    | Invalid String


validateMap : (to -> Result String toMapped) -> CliUnit from to -> CliUnit from toMapped
validateMap function tofromCliUnit =
    Debug.crash ""


validate : (to -> ValidationResult) -> CliUnit from to -> CliUnit from to
validate validateFunction (CliUnit dataGrabber usageSpec (Cli.Decode.Decoder decodeFn)) =
    CliUnit dataGrabber
        usageSpec
        (Cli.Decode.Decoder
            (decodeFn
                >> (\result ->
                        Result.map
                            (\( validationErrors, value ) ->
                                case validateFunction value of
                                    Valid ->
                                        ( validationErrors, value )

                                    Invalid invalidReason ->
                                        ( validationErrors
                                            ++ [ { name = Cli.UsageSpec.name usageSpec
                                                 , invalidReason = invalidReason
                                                 , valueAsString = toString value
                                                 }
                                               ]
                                        , value
                                        )
                            )
                            result
                   )
            )
        )


validateIfPresent : (to -> ValidationResult) -> CliUnit from (Maybe to) -> CliUnit from (Maybe to)
validateIfPresent validateFunction cliUnit =
    validate
        (\maybeValue ->
            case maybeValue of
                Just value ->
                    validateFunction value

                Nothing ->
                    Valid
        )
        cliUnit


expect : Expectation -> CommandBuilder decodesTo -> CommandBuilder decodesTo
expect expectation builder =
    -- CommandBuilder { decoder = \record -> _, usageSpecs = [], description = Nothing }
    builder


with : CliUnit from to -> CommandBuilder (to -> msg) -> CommandBuilder msg
with (CliUnit dataGrabber usageSpec (Cli.Decode.Decoder decodeFn)) ((CommandBuilder ({ decoder, usageSpecs } as command)) as fullCommand) =
    CommandBuilder
        { command
            | decoder =
                \optionsAndOperands ->
                    { options = optionsAndOperands.options
                    , operands = optionsAndOperands.operands
                    , usageSpecs = optionsAndOperands.usageSpecs
                    , operandsSoFar = operandCount usageSpecs
                    }
                        |> dataGrabber
                        |> Result.andThen decodeFn
                        |> Result.andThen
                            (\( validationErrors, fromValue ) ->
                                case
                                    resultMap (\fn -> fn fromValue)
                                        (decoder optionsAndOperands)
                                of
                                    Ok ( previousValidationErrors, thing ) ->
                                        Ok ( previousValidationErrors ++ validationErrors, thing )

                                    value ->
                                        value
                            )
            , usageSpecs = usageSpecs ++ [ usageSpec ]
        }


optionName : Option -> String
optionName option =
    case option of
        Flag flagName ->
            flagName

        OptionWithStringArg optionName ->
            optionName
