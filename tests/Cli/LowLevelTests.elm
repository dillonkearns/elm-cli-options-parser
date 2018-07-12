module Cli.LowLevelTests exposing (all)

import Cli.Command as Command
import Cli.LowLevel
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
                Cli.LowLevel.try cli [ "", "", "--help" ]
                    |> Expect.equal (Cli.LowLevel.Match 123)
        , test "non-matching command" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        ]
                in
                Cli.LowLevel.try cli [ "", "" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [])
        , test "unknown flag" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        ]
                in
                Cli.LowLevel.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [ "unknown-flag" ])
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
                Cli.LowLevel.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [ "unknown-flag" ])
        , test "help" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "version"
                            |> Command.toCommand
                        ]
                in
                Cli.LowLevel.try cli [ "", "", "--help" ]
                    |> Expect.equal Cli.LowLevel.ShowHelp
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
                Cli.LowLevel.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [ "unknown-flag" ])
        ]
