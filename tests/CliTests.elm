module CliTests exposing (all)

import Cli
import Cli.Command as Command
import Expect exposing (Expectation)
import Test exposing (..)


all : Test
all =
    describe "synopsis"
        [ test "matching command" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        ]
                in
                Cli.try cli [ "", "", "--help" ]
                    |> Expect.equal (Cli.Match 123)
        , test "non-matching command" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        ]
                in
                Cli.try cli [ "", "" ]
                    |> Expect.equal (Cli.NoMatch [])
        , test "unknown flag" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        ]
                in
                Cli.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.NoMatch [ "unknown-flag" ])
        , test "unknown flag with multiple usage specs" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        , Command.build 456
                            |> Command.expectFlag "version"
                            |> Command.toCommand
                        ]
                in
                Cli.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.NoMatch [ "unknown-flag" ])
        , test "help" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "version"
                            |> Command.toCommand
                        ]
                in
                Cli.try cli [ "", "", "--help" ]
                    |> Expect.equal Cli.ShowHelp
        , test "unknown flag with a subcommand spec" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        , Command.subCommand "sub" 456
                            |> Command.toCommand
                        ]
                in
                Cli.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.NoMatch [ "unknown-flag" ])
        ]
