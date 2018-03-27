module Command exposing (Command, Format(..), ParserError(..), command, commandWithArg, tryMatch, withFlag)

import Json.Decode as Decode


tryMatch : Command msg -> List String -> Maybe msg
tryMatch (Command decoder format) argv =
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


type Command msg
    = Command (Decode.Decoder msg) Format


command : msg -> Format -> Command msg
command msg format =
    Command (Decode.succeed msg) format


commandWithArg : (String -> msg) -> Command msg
commandWithArg msg =
    Command (Decode.map msg (Decode.index 0 Decode.string)) OperandOnly


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


type Format
    = LongOnly String
    | OperandOnly


type ParserError
    = UnknownOption String
