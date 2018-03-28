module Command exposing (Command, Format(..), ParserError(..), command, commandWithArg, empty, optionWithStringArg, synopsis, tryMatch, withFlag)

import Json.Decode as Decode
import List.Extra


tryMatch : List String -> Command msg -> Maybe msg
tryMatch argv (Command decoder format) =
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
    = Command (Decode.Decoder msg) Format


command : msg -> Format -> Command msg
command msg format =
    Command (Decode.succeed msg) format


empty : (a -> msg) -> Command (a -> msg)
empty msgConstructor =
    Command (Decode.succeed msgConstructor) Empty


commandWithArg : (String -> msg) -> Command msg
commandWithArg msg =
    Command (Decode.map msg (Decode.index 0 Decode.string)) OperandOnly


synopsis : String -> Command msg -> String
synopsis programName (Command decoder format) =
    case format of
        LongOnly longOption ->
            programName ++ " --" ++ longOption

        OperandOnly ->
            "TODO"

        Empty ->
            "TODO"


withFlag : String -> Command (Bool -> msg) -> Command msg
withFlag flag (Command msgConstructor format) =
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


optionWithStringArg : String -> Command (String -> msg) -> Command msg
optionWithStringArg flag (Command msgConstructor format) =
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


type Format
    = LongOnly String
    | OperandOnly
    | Empty


type ParserError
    = UnknownOption String
