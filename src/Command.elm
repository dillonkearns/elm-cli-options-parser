module Command exposing (Command, Format(..), ParserError(..), build, commandWithArg, expectFlag, optionWithStringArg, synopsis, tryMatch, withFlag)

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


build : msg -> Command msg
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
        (options ++ [ Flag flagName ])


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
