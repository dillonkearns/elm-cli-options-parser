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
                Command.tryMatch [ "--help" ] (Command.command Help (Command.LongOnly "help"))
                    |> Expect.equal (Just Help)
        , test "version command" <|
            \() ->
                Command.tryMatch [ "--version" ] (Command.command Version (Command.LongOnly "version"))
                    |> Expect.equal (Just Version)
        , test "matching non-first element in list" <|
            \() ->
                Command.tryMatch [ "unused", "--version" ] (Command.command Version (Command.LongOnly "version"))
                    |> Expect.equal Nothing
        , test "command with args" <|
            \() ->
                Command.tryMatch [ "http://my-domain.com" ] (Command.commandWithArg OpenUrl)
                    |> Expect.equal (Just (OpenUrl "http://my-domain.com"))
        , test "detects that optional flag is absent" <|
            \() ->
                Command.tryMatch [ "http://my-domain.com" ] (Command.commandWithArg OpenUrlWithFlag |> Command.withFlag "p")
                    |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" False))
        , test "detects that optional flag is present" <|
            \() ->
                Command.tryMatch [ "http://my-domain.com", "-p" ] (Command.commandWithArg OpenUrlWithFlag |> Command.withFlag "p")
                    |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" True))
        , test "non-matching option" <|
            \() ->
                Command.tryMatch [ "--version" ] (Command.command Help (Command.LongOnly "help"))
                    |> Expect.equal Nothing
        , test "print synopsis with required flag" <|
            \() ->
                Command.command Version (Command.LongOnly "version")
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --version"

        -- |> Expect.equal "greet -n <name> [-l][-a][-c option_argument][operand...]"
        ]
