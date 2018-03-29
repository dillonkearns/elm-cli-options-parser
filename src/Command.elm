module Command exposing (Command, Format(..), ParserError(..), build, command, commandWithArg, optionWithStringArg, synopsis, tryMatch, withFlag)

import Json.Decode as Decode
import List.Extra


tryMatch : List String -> Command msg -> Maybe msg
tryMatch argv (Command decoder format options) =
    case format of
        LongOnly longOption ->
            if argv == [ "--" ++ longOption ] then
                Decode.decodeString decoder (argv |> toString)
                    |> Result.toMaybe
            else
                Nothing

        OperandOnly ->
            Decode.decodeString decoder (argv |> toString)
                |> Result.toMaybe

        Empty ->
            Decode.decodeString decoder (argv |> toString)
                |> Result.toMaybe


type Command msg
    = Command (Decode.Decoder msg) Format (List Option)


command : msg -> Format -> Command msg
command msg format =
    Command (Decode.succeed msg) format []


build : (a -> msg) -> Command (a -> msg)
build msgConstructor =
    Command (Decode.succeed msgConstructor) Empty []


commandWithArg : (String -> msg) -> Command msg
commandWithArg msg =
    Command (Decode.map msg (Decode.index 0 Decode.string)) OperandOnly []


synopsis : String -> Command msg -> String
synopsis programName (Command decoder format options) =
    case format of
        LongOnly longOption ->
            programName ++ " --" ++ longOption

        OperandOnly ->
            "TODO"

        Empty ->
            programName
                ++ " "
                ++ (options |> List.map optionSynopsis |> String.join " ")



-- " --first-name <first-name> --last-name <last-name>"


optionSynopsis : Option -> String
optionSynopsis option =
    case option of
        Flag string ->
            ""

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
        (options ++ [ OptionWithStringArg flag ])


type Format
    = LongOnly String
    | OperandOnly
    | Empty


type Option
    = Flag String
    | OptionWithStringArg String


type ParserError
    = UnknownOption String
