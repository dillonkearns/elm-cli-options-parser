module Command exposing (Command, Format(..), ParserError(..), build, commandWithArg, expectFlag, flagsAndOperands, optionWithStringArg, synopsis, tryMatch, withFlag)

import Json.Decode as Decode exposing (Decoder)
import List.Extra


tryMatch : List String -> Command msg -> Maybe msg
tryMatch argv ((Command decoder format options) as command) =
    case format of
        OperandOnly ->
            Decode.decodeString
                (flagsAndOperandsAndThen command
                    (\{ operands } ->
                        if List.length operands > 1 then
                            Decode.fail "More operands than expected"
                        else
                            decoder
                    )
                )
                (argv |> toString)
                |> Result.toMaybe

        Empty ->
            Decode.decodeString
                (flagsAndOperandsAndThen command
                    (\{ operands } ->
                        if List.length operands > 0 then
                            Decode.fail "More operands than expected"
                        else
                            decoder
                    )
                )
                (argv |> toString)
                |> Result.toMaybe


type Command msg
    = Command (Decode.Decoder msg) Format (List UsageSpec)


build : msg -> Command msg
build msgConstructor =
    Command (Decode.succeed msgConstructor) Empty []


commandWithArg : (String -> msg) -> Command msg
commandWithArg msg =
    Command (Decode.map msg (Decode.index 0 Decode.string)) OperandOnly []


synopsis : String -> Command msg -> String
synopsis programName (Command decoder format options) =
    case format of
        OperandOnly ->
            "TODO"

        Empty ->
            programName
                ++ " "
                ++ (options
                        |> List.filterMap
                            (\spec ->
                                case spec of
                                    Option option ->
                                        Just option
                            )
                        |> List.map optionSynopsis
                        |> String.join " "
                   )


optionSynopsis : Option -> String
optionSynopsis option =
    case option of
        Flag flagName ->
            "--" ++ flagName

        OptionWithStringArg optionName ->
            "--" ++ optionName ++ " <" ++ optionName ++ ">"


withFlag : String -> Command (Bool -> msg) -> Command msg
withFlag flag (Command msgConstructor format options) =
    Command
        (Decode.list Decode.string
            |> Decode.andThen
                (\list ->
                    if List.member ("-" ++ flag) list then
                        Decode.map (\constructor -> constructor True) msgConstructor
                    else
                        Decode.map (\constructor -> constructor False) msgConstructor
                )
        )
        format
        options


expectFlag : String -> Command msg -> Command msg
expectFlag flagName (Command decoder format options) =
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
        format
        (options ++ [ Flag flagName |> Option ])


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
optionWithStringArg flag (Command msgConstructor format options) =
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
        format
        (options ++ [ OptionWithStringArg flag |> Option ])


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
                        Option option ->
                            Just option
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
flagsAndOperands (Command msgConstructor format options) argv =
    let
        firstOptionIndex =
            argv
                |> List.Extra.findIndex isFlag
                |> Maybe.withDefault 0

        lastOptionIndex =
            List.Extra.indexedFoldl
                (\index element lastIndexSoFar ->
                    let
                        hasArg =
                            optionHasArg options (String.dropLeft 2 element)
                    in
                    if index < firstOptionIndex then
                        lastIndexSoFar
                    else if isFlag element then
                        if hasArg then
                            index + 1
                        else
                            index
                    else
                        lastIndexSoFar
                )
                firstOptionIndex
                argv

        frontOperands =
            argv
                |> List.Extra.splitAt firstOptionIndex
                |> Tuple.first

        withoutFrontOperands =
            argv
                |> List.Extra.splitAt firstOptionIndex
                |> Tuple.second

        ( flags, backOperands ) =
            withoutFrontOperands
                |> List.Extra.splitAt (lastOptionIndex + 1 - (List.length argv - List.length withoutFrontOperands))
    in
    { flags = flags, operands = frontOperands ++ backOperands }


type Format
    = OperandOnly
    | Empty



-- foo =
--     List.Extra.find (\option -> optionName option == String.dropLeft 2 element) options


type Option
    = Flag String
    | OptionWithStringArg String


type UsageSpec
    = Option Option


optionName : Option -> String
optionName option =
    case option of
        Flag flagName ->
            flagName

        OptionWithStringArg optionName ->
            optionName


type ParserError
    = UnknownOption String
