module Command exposing (Command, CommandBuilder, build, buildWithDoc, captureRestOperands, expectFlag, expectOperandNew, flagsAndOperands, getUsageSpecs, mapNew, optionalListOption, optionalOption, requiredOptionNew, synopsis, toCommand, tryMatchNew, validate, with, withFlagNew)

import Cli.Decode
import Cli.UsageSpec exposing (..)
import Json.Decode as Decode exposing (Decoder)
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


tryMatchNew : List String -> Command msg -> Maybe msg
tryMatchNew argv ((Command { newDecoder, usageSpecs }) as command) =
    let
        decoder =
            command
                |> expectedOperandCountOrFailNew
                |> failIfUnexpectedOptionsNew
                |> getDecoderNew
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


expectedOperandCountOrFailNew : Command msg -> Command msg
expectedOperandCountOrFailNew ((Command ({ newDecoder, usageSpecs } as command)) as fullCommand) =
    Command
        { command
            | newDecoder =
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
                        newDecoder stuff
            , usageSpecs = usageSpecs
        }


decoder : Command msg -> Decoder msg
decoder (Command { decoder }) =
    decoder


getDecoderNew :
    Command msg
    ->
        { operands : List String
        , options : List ParsedOption
        , usageSpecs : List UsageSpec
        }
    -> Result String msg
getDecoderNew (Command { newDecoder }) =
    newDecoder


failIfUnexpectedOptionsNew : Command msg -> Command msg
failIfUnexpectedOptionsNew ((Command ({ newDecoder, decoder, usageSpecs } as command)) as fullCommand) =
    Command
        { command
            | newDecoder =
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
                        newDecoder stuff
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
        { decoder : Decode.Decoder msg
        , newDecoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result String msg
        , usageSpecs : List UsageSpec
        , description : Maybe String
        }


toCommand : CommandBuilder msg -> Command msg
toCommand (CommandBuilder record) =
    Command record


captureRestOperands : String -> CommandBuilder (List String -> msg) -> Command msg
captureRestOperands restOperandsDescription (CommandBuilder ({ decoder, usageSpecs, description, newDecoder } as record)) =
    Command
        { decoder =
            flagsAndOperandsAndThen (Command { record | newDecoder = \_ -> Err "" })
                (\{ operands } ->
                    Decode.map
                        (\constructor ->
                            operands
                                |> List.drop (operandCount usageSpecs)
                                |> constructor
                        )
                        decoder
                )
        , usageSpecs = usageSpecs ++ [ RestArgs restOperandsDescription ]
        , description = description
        , newDecoder =
            \({ operands } as stuff) ->
                let
                    restOperands =
                        operands
                            |> List.drop (operandCount usageSpecs)
                in
                Result.map (\fn -> fn restOperands) (newDecoder stuff)
        }


type Command msg
    = Command
        { decoder : Decode.Decoder msg
        , newDecoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result String msg
        , usageSpecs : List UsageSpec
        , description : Maybe String
        }


build : msg -> CommandBuilder msg
build msgConstructor =
    CommandBuilder
        { decoder = Decode.succeed msgConstructor
        , usageSpecs = []
        , description = Nothing
        , newDecoder = \_ -> Ok msgConstructor
        }


buildWithDoc : msg -> String -> CommandBuilder msg
buildWithDoc msgConstructor docString =
    CommandBuilder
        { decoder = Decode.succeed msgConstructor
        , usageSpecs = []
        , description = Just docString
        , newDecoder = \_ -> Ok msgConstructor
        }


expectFlag : String -> CommandBuilder msg -> CommandBuilder msg
expectFlag flagName (CommandBuilder ({ decoder, usageSpecs, newDecoder } as command)) =
    let
        formattedFlag =
            "--" ++ flagName
    in
    CommandBuilder
        { command
            | decoder =
                flagsAndThen
                    (\list ->
                        if
                            list
                                |> List.member formattedFlag
                        then
                            decoder
                        else
                            ("Expect flag " ++ formattedFlag)
                                |> Decode.fail
                    )
            , usageSpecs = usageSpecs ++ [ Option (Flag flagName) Required ]
            , newDecoder =
                \({ options } as stuff) ->
                    if
                        options
                            |> List.member (Parser.ParsedOption flagName Parser.Flag)
                    then
                        newDecoder stuff
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


flagsAndThen : (List String -> Decode.Decoder a) -> Decode.Decoder a
flagsAndThen something =
    let
        startsWithFlag list =
            list |> List.head |> Maybe.map isFlag |> Maybe.withDefault True
    in
    Decode.list Decode.string
        |> Decode.andThen
            (\list ->
                if startsWithFlag list then
                    something list
                else
                    something []
            )


isFlag : String -> Bool
isFlag string =
    string |> String.startsWith "--"


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
                                Nothing
                    )
                |> Ok
        )
        (Option (OptionWithStringArg flagName) ZeroOrMore)
        Cli.Decode.decoder


flagsAndOperandsAndThen : Command msg -> ({ usageSpecs : List UsageSpec, flags : List String, operands : List String, options : List Parser.ParsedOption } -> Decoder decodesTo) -> Decoder decodesTo
flagsAndOperandsAndThen command decoderFunction =
    Decode.list Decode.string
        |> Decode.andThen
            (\list ->
                list
                    |> flagsAndOperands command
                    |> (\{ usageSpecs, flags, operands } ->
                            { usageSpecs = usageSpecs
                            , flags = flags
                            , operands = operands
                            , options =
                                (Parser.flagsAndOperands (command |> getUsageSpecs)
                                    list
                                ).options
                            }
                       )
                    |> decoderFunction
            )


optionHasArg : List UsageSpec -> String -> Bool
optionHasArg options optionNameToCheck =
    case
        options
            |> List.filterMap
                (\spec ->
                    case spec of
                        Option option occurences ->
                            Just option

                        Operand _ ->
                            Nothing

                        RestArgs _ ->
                            Nothing
                )
            |> List.Extra.find
                (\spec -> optionName spec == optionNameToCheck)
    of
        Just option ->
            case option of
                Flag flagName ->
                    False

                OptionWithStringArg optionName ->
                    True

        Nothing ->
            False


flagsAndOperands : Command msg -> List String -> { usageSpecs : List UsageSpec, flags : List String, operands : List String }
flagsAndOperands (Command { usageSpecs }) argv =
    let
        firstOptionIndex =
            argv
                |> List.Extra.findIndex isFlag
    in
    case firstOptionIndex of
        Just firstIndex ->
            let
                lastOptionIndex =
                    List.Extra.indexedFoldl
                        (\index element lastIndexSoFar ->
                            let
                                hasArg =
                                    optionHasArg usageSpecs (String.dropLeft 2 element)
                            in
                            if index < firstIndex then
                                lastIndexSoFar
                            else if isFlag element then
                                if hasArg then
                                    index + 1
                                else
                                    index
                            else
                                lastIndexSoFar
                        )
                        firstIndex
                        argv

                frontOperands =
                    argv
                        |> List.Extra.splitAt firstIndex
                        |> Tuple.first

                withoutFrontOperands =
                    argv
                        |> List.Extra.splitAt firstIndex
                        |> Tuple.second

                ( flags, backOperands ) =
                    withoutFrontOperands
                        |> List.Extra.splitAt (lastOptionIndex + 1 - (List.length argv - List.length withoutFrontOperands))
            in
            { flags = flags, operands = frontOperands ++ backOperands, usageSpecs = usageSpecs }

        Nothing ->
            { flags = [], operands = argv, usageSpecs = usageSpecs }


type CliUnit from to
    = CliUnit (DataGrabber from) UsageSpec (Cli.Decode.Decoder from to)


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec, operands : List String, options : List Parser.ParsedOption, operandsSoFar : Int } -> Result String decodesTo


expectOperandNew : String -> CliUnit String String
expectOperandNew operandDescription =
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


requiredOptionNew : String -> CliUnit String String
requiredOptionNew optionName =
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


withFlagNew : String -> CliUnit Bool Bool
withFlagNew flagName =
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


validate : (to -> Result String to) -> CliUnit from to -> CliUnit from to
validate validateFunction (CliUnit dataGrabber usageSpec (Cli.Decode.Decoder decodeFn)) =
    CliUnit dataGrabber
        usageSpec
        (Cli.Decode.Decoder
            (decodeFn
                >> (\result ->
                        case result of
                            Ok value ->
                                validateFunction value

                            Err error ->
                                result
                   )
            )
        )


with : CliUnit from to -> CommandBuilder (to -> msg) -> CommandBuilder msg
with (CliUnit dataGrabber usageSpec (Cli.Decode.Decoder decodeFn)) ((CommandBuilder ({ newDecoder, decoder, usageSpecs } as command)) as fullCommand) =
    CommandBuilder
        { command
            | decoder = Decode.fail "Hello"
            , newDecoder =
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
                                    (newDecoder optionsAndOperands)
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
