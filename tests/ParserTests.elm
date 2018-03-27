module ParserTests exposing (all)

import Expect exposing (Expectation)
import Json.Decode as Decode
import Test exposing (..)


type Msg
    = Help
    | Version
    | OpenUrl String


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


type Format
    = LongOnly String
    | OperandOnly


type ParserError
    = UnknownOption String


all : Test
all =
    describe "CLI options parser"
        [ test "help command" <|
            \() ->
                [ "--help" ]
                    |> tryMatch (command Help (LongOnly "help"))
                    |> Expect.equal (Just Help)
        , test "version command" <|
            \() ->
                [ "--version" ]
                    |> tryMatch (command Version (LongOnly "version"))
                    |> Expect.equal (Just Version)
        , test "matching non-first element in list" <|
            \() ->
                [ "unused", "--version" ]
                    |> tryMatch (command Version (LongOnly "version"))
                    |> Expect.equal Nothing
        , test "command with args" <|
            \() ->
                [ "http://my-domain.com" ]
                    |> tryMatch (commandWithArg OpenUrl)
                    |> Expect.equal (Just (OpenUrl "http://my-domain.com"))
        , test "non-matching option" <|
            \() ->
                [ "--version" ]
                    |> tryMatch (command Help (LongOnly "help"))
                    |> Expect.equal Nothing
        ]
