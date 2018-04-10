module Command exposing (Command, CommandBuilder, ValidationResult(..), build, buildWithDoc, captureRestOperands, expectFlag, getUsageSpecs, hardcoded, mapNew, optionalFlag, optionalListOption, optionalOption, requiredOperand, requiredOption, synopsis, toCommand, tryMatch, validate, with, withDefault)

import Cli.Decode
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


tryMatch : List String -> Command msg -> Maybe msg
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
        |> Result.toMaybe


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
    -> Result String msg
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
        { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result String msg
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
                Result.map (\fn -> fn restOperands) (decoder stuff)
        }


type Command msg
    = Command
        { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result String msg
        , usageSpecs : List UsageSpec
        , description : Maybe String
        }


build : msg -> CommandBuilder msg
build msgConstructor =
    CommandBuilder
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok msgConstructor
        }


buildWithDoc : msg -> String -> CommandBuilder msg
buildWithDoc msgConstructor docString =
    CommandBuilder
        { usageSpecs = []
        , description = Just docString
        , decoder = \_ -> Ok msgConstructor
        }


hardcoded : value -> CommandBuilder (value -> msg) -> CommandBuilder msg
hardcoded hardcodedValue (CommandBuilder ({ decoder } as command)) =
    CommandBuilder
        { command
            | decoder =
                \stuff -> Result.map (\fn -> fn hardcodedValue) (decoder stuff)
        }


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


optionalListOption : String -> CliUnit (List String) (List String)
optionalListOption flagName =
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


type CliUnit from to
    = CliUnit (DataGrabber from) UsageSpec (Cli.Decode.Decoder from to)


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec, operands : List String, options : List Parser.ParsedOption, operandsSoFar : Int } -> Result String decodesTo


requiredOperand : String -> CliUnit String String
requiredOperand operandDescription =
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


optionalOption : String -> CliUnit (Maybe String) (Maybe String)
optionalOption optionName =
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


requiredOption : String -> CliUnit String String
requiredOption optionName =
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


optionalFlag : String -> CliUnit Bool Bool
optionalFlag flagName =
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


validate : (to -> ValidationResult) -> CliUnit from to -> CliUnit from to
validate validateFunction (CliUnit dataGrabber usageSpec (Cli.Decode.Decoder decodeFn)) =
    CliUnit dataGrabber
        usageSpec
        (Cli.Decode.Decoder
            (decodeFn
                >> (\result ->
                        case result of
                            Ok value ->
                                case validateFunction value of
                                    Valid ->
                                        result

                                    Invalid invalidReason ->
                                        Err invalidReason

                            Err error ->
                                result
                   )
            )
        )


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
                            (\fromValue ->
                                Result.map (\fn -> fn fromValue)
                                    (decoder optionsAndOperands)
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
