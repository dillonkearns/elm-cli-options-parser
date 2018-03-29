module Command exposing (Command, build, expectFlag, expectOperand, flagsAndOperands, optionWithStringArg, optionalOptionWithStringArg, synopsis, tryMatch, withFlag)

import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Occurences exposing (Occurences(..))


tryMatch : List String -> Command msg -> Maybe msg
tryMatch argv command =
    Decode.decodeString
        (command
            |> expectedOperandCountOrFail
            |> failIfUnexpectedOptions
            |> decoder
        )
        (argv |> toString)
        |> Result.toMaybe


expectedOperandCountOrFail : Command msg -> Command msg
expectedOperandCountOrFail ((Command decoder options) as command) =
    Command
        (flagsAndOperandsAndThen command
            (\{ operands } ->
                if
                    (operands |> List.length)
                        > (options
                            |> List.filterMap
                                (\option ->
                                    case option of
                                        Operand operand ->
                                            Just operand

                                        Option _ _ ->
                                            Nothing
                                )
                            |> List.length
                          )
                then
                    Decode.fail "More operands than expected"
                else
                    decoder
            )
        )
        options


decoder : Command msg -> Decoder msg
decoder (Command decoder options) =
    decoder


failIfUnexpectedOptions : Command msg -> Command msg
failIfUnexpectedOptions ((Command decoder options) as command) =
    Command
        (flagsAndOperandsAndThen command
            (\{ flags } ->
                let
                    ( invalidOptions, unconsumedArg ) =
                        flags
                            |> List.Extra.indexedFoldl
                                (\index element ( invalidSoFar, unconsumedLeft ) ->
                                    if unconsumedLeft then
                                        ( invalidSoFar, False )
                                    else
                                        case optionExists options element of
                                            Just option ->
                                                case option of
                                                    OptionWithStringArg argName ->
                                                        ( invalidSoFar, True )

                                                    Flag flagName ->
                                                        ( invalidSoFar, False )

                                            Nothing ->
                                                ( invalidSoFar ++ [ element ], False )
                                )
                                ( [], False )
                in
                if invalidOptions == [] && not unconsumedArg then
                    decoder
                else
                    Decode.fail "Found unexpected options."
            )
        )
        options


optionExists : List UsageSpec -> String -> Maybe Option
optionExists usageSpecs thisOptionName =
    usageSpecs
        |> List.filterMap
            (\usageSpec ->
                case usageSpec of
                    Option option occurences ->
                        option
                            |> Just

                    Operand _ ->
                        Nothing
            )
        |> List.Extra.find (\option -> ("--" ++ optionName option) == thisOptionName)


type Command msg
    = Command (Decode.Decoder msg) (List UsageSpec)


build : msg -> Command msg
build msgConstructor =
    Command (Decode.succeed msgConstructor) []


synopsis : String -> Command msg -> String
synopsis programName (Command decoder options) =
    programName
        ++ " "
        ++ (options
                |> List.map
                    (\spec ->
                        case spec of
                            Option option occurences ->
                                optionSynopsis occurences option

                            Operand operandName ->
                                "<" ++ operandName ++ ">"
                    )
                |> String.join " "
           )


optionSynopsis : Occurences -> Option -> String
optionSynopsis occurences option =
    (case option of
        Flag flagName ->
            "--" ++ flagName

        OptionWithStringArg optionName ->
            "--" ++ optionName ++ " <" ++ optionName ++ ">"
    )
        |> Occurences.qualifySynopsis occurences


withFlag : String -> Command (Bool -> msg) -> Command msg
withFlag flagName (Command msgConstructor options) =
    Command
        (Decode.list Decode.string
            |> Decode.andThen
                (\list ->
                    if List.member ("--" ++ flagName) list then
                        Decode.map (\constructor -> constructor True) msgConstructor
                    else
                        Decode.map (\constructor -> constructor False) msgConstructor
                )
        )
        (options ++ [ Option (Flag flagName) Optional ])


expectFlag : String -> Command msg -> Command msg
expectFlag flagName (Command decoder options) =
    let
        formattedFlag =
            "--" ++ flagName
    in
    Command
        (flagsAndThen
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
        )
        (options ++ [ Option (Flag flagName) Required ])


expectOperand : String -> Command (String -> msg) -> Command msg
expectOperand operandName ((Command decoder options) as command) =
    Command
        (flagsAndOperandsAndThen command
            (\{ operands } ->
                let
                    operandsSoFar =
                        options
                            |> List.filterMap
                                (\spec ->
                                    case spec of
                                        Option _ _ ->
                                            Nothing

                                        Operand operandName ->
                                            Just operandName
                                )
                            |> List.length
                in
                case
                    operands
                        |> List.Extra.getAt operandsSoFar
                of
                    Just operandValue ->
                        Decode.map
                            (\constructor -> constructor operandValue)
                            decoder

                    Nothing ->
                        ("Expect operand " ++ operandName)
                            |> Decode.fail
            )
        )
        (options ++ [ Operand operandName ])


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


optionWithStringArg : String -> Command (String -> msg) -> Command msg
optionWithStringArg flag (Command msgConstructor options) =
    Command
        (Decode.list Decode.string
            |> Decode.andThen
                (\list ->
                    case list |> List.Extra.elemIndex ("--" ++ flag) of
                        Nothing ->
                            Decode.fail ("--" ++ flag ++ " not found")

                        Just flagIndex ->
                            case list |> List.Extra.getAt (flagIndex + 1) of
                                Nothing ->
                                    Decode.fail ("Found --" ++ flag ++ " flag but expected an argument")

                                Just argValue ->
                                    Decode.map (\constructor -> constructor argValue) msgConstructor
                )
        )
        (options ++ [ Option (OptionWithStringArg flag) Required ])


optionalOptionWithStringArg : String -> Command (Maybe String -> msg) -> Command msg
optionalOptionWithStringArg flag (Command msgConstructor options) =
    Command
        (Decode.list Decode.string
            |> Decode.andThen
                (\list ->
                    case list |> List.Extra.elemIndex ("--" ++ flag) of
                        Nothing ->
                            Decode.map (\constructor -> constructor Nothing) msgConstructor

                        Just flagIndex ->
                            case list |> List.Extra.getAt (flagIndex + 1) of
                                Nothing ->
                                    Decode.fail ("Found --" ++ flag ++ " flag but expected an argument")

                                Just argValue ->
                                    Decode.map (\constructor -> constructor (Just argValue)) msgConstructor
                )
        )
        (options ++ [ Option (OptionWithStringArg flag) Optional ])


flagsAndOperandsAndThen : Command msg -> ({ flags : List String, operands : List String } -> Decoder decodesTo) -> Decoder decodesTo
flagsAndOperandsAndThen command decoderFunction =
    Decode.list Decode.string
        |> Decode.andThen
            (\list ->
                list
                    |> flagsAndOperands command
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


flagsAndOperands : Command msg -> List String -> { flags : List String, operands : List String }
flagsAndOperands (Command msgConstructor options) argv =
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
                                    optionHasArg options (String.dropLeft 2 element)
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
            { flags = flags, operands = frontOperands ++ backOperands }

        Nothing ->
            { flags = [], operands = argv }


type Option
    = Flag String
    | OptionWithStringArg String


type UsageSpec
    = Option Option Occurences
    | Operand String


optionName : Option -> String
optionName option =
    case option of
        Flag flagName ->
            flagName

        OptionWithStringArg optionName ->
            optionName
