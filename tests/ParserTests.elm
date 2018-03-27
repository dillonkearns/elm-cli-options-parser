module ParserTests exposing (all)

import Command
import Expect exposing (Expectation)
import Test exposing (..)


type Msg
    = Help
    | Version
    | OpenUrl String
    | OpenUrlWithFlag String Bool


all : Test
all =
    describe "CLI options parser"
        [ test "help command" <|
            \() ->
                [ "--help" ]
                    |> Command.tryMatch (Command.command Help (Command.LongOnly "help"))
                    |> Expect.equal (Just Help)
        , test "version command" <|
            \() ->
                [ "--version" ]
                    |> Command.tryMatch (Command.command Version (Command.LongOnly "version"))
                    |> Expect.equal (Just Version)
        , test "matching non-first element in list" <|
            \() ->
                [ "unused", "--version" ]
                    |> Command.tryMatch (Command.command Version (Command.LongOnly "version"))
                    |> Expect.equal Nothing
        , test "command with args" <|
            \() ->
                [ "http://my-domain.com" ]
                    |> Command.tryMatch (Command.commandWithArg OpenUrl)
                    |> Expect.equal (Just (OpenUrl "http://my-domain.com"))
        , test "detects that optional flag is absent" <|
            \() ->
                [ "http://my-domain.com" ]
                    |> Command.tryMatch (Command.commandWithArg OpenUrlWithFlag |> Command.withFlag "p")
                    |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" False))
        , test "detects that optional flag is present" <|
            \() ->
                [ "http://my-domain.com", "-p" ]
                    |> Command.tryMatch (Command.commandWithArg OpenUrlWithFlag |> Command.withFlag "p")
                    |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" True))
        , test "non-matching option" <|
            \() ->
                [ "--version" ]
                    |> Command.tryMatch (Command.command Help (Command.LongOnly "help"))
                    |> Expect.equal Nothing
        ]
