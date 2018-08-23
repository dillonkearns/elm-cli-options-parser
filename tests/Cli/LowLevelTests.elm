module Cli.LowLevelTests exposing (all)

import Cli.LowLevel
import Cli.OptionsParser as OptionsParser
import Expect exposing (Expectation)
import Test exposing (..)


all : Test
all =
    describe "synopsis"
        [ test "matching optionsParser" <|
            \() ->
                let
                    cli =
                        [ OptionsParser.build 123
                            |> OptionsParser.expectFlag "help"
                            |> OptionsParser.end
                        ]
                in
                Cli.LowLevel.try cli [ "", "", "--help" ]
                    |> Expect.equal (Cli.LowLevel.Match 123)
        , test "non-matching optionsParser" <|
            \() ->
                let
                    cli =
                        [ OptionsParser.build 123
                            |> OptionsParser.expectFlag "help"
                            |> OptionsParser.end
                        ]
                in
                Cli.LowLevel.try cli [ "", "" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [])
        , test "unknown flag" <|
            \() ->
                let
                    cli =
                        [ OptionsParser.build 123
                            |> OptionsParser.expectFlag "help"
                            |> OptionsParser.end
                        ]
                in
                Cli.LowLevel.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [ "unknown-flag" ])
        , test "unknown flag with multiple usage specs" <|
            \() ->
                let
                    cli =
                        [ OptionsParser.build 123
                            |> OptionsParser.expectFlag "help"
                            |> OptionsParser.end
                        , OptionsParser.build 456
                            |> OptionsParser.expectFlag "version"
                            |> OptionsParser.end
                        ]
                in
                Cli.LowLevel.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [ "unknown-flag" ])
        , test "help" <|
            \() ->
                let
                    cli =
                        [ OptionsParser.build 123
                            |> OptionsParser.expectFlag "version"
                            |> OptionsParser.end
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
        , test "unknown flag with a suboptionsParser spec" <|
            \() ->
                let
                    cli =
                        [ OptionsParser.build 123
                            |> OptionsParser.expectFlag "help"
                            |> OptionsParser.end
                        , OptionsParser.buildSubCommand "sub" 456
                            |> OptionsParser.end
                        ]
                in
                Cli.LowLevel.try cli [ "", "", "--unknown-flag" ]
                    |> Expect.equal (Cli.LowLevel.NoMatch [ "unknown-flag" ])
        ]
