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
                            |> Command.end
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
                            |> Command.end
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
                            |> Command.end
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
                            |> Command.end
                        , Command.build 456
                            |> Command.expectFlag "version"
                            |> Command.end
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
                            |> Command.end
                        ]
                in
                Cli.LowLevel.try cli [ "", "", "--help" ]
                    |> Expect.equal Cli.LowLevel.ShowHelp
        , test "version" <|
            \() ->
                let
                    cli =
                        []
                in
                Cli.LowLevel.try cli [ "", "", "--version" ]
                    |> Expect.equal Cli.LowLevel.ShowVersion
        , test "unknown flag with a subcommand spec" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.end
                        , Command.buildSubCommand "sub" 456
                            |> Command.end
                        ]
                in
                Cli.LowLevel.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [ "unknown-flag" ])
        ]
