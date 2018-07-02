module TypoSuggestionTests exposing (all)

import Cli.Command as Command
import Expect exposing (Expectation)
import Test exposing (..)
import TypoSuggestion


all : Test
all =
    describe "typo suggestions"
        [ test "exact matching subcommand" <|
            \() ->
                let
                    cli =
                        [ Command.subCommand "sub" 456
                            |> Command.toCommand
                        ]
                in
                TypoSuggestion.getSuggestions cli "sub"
                    |> Expect.equal [ TypoSuggestion.SubCommand "sub" ]
        , test "letter swapped in flag name" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "unrelated"
                            |> Command.toCommand
                        , Command.build 123
                            |> Command.expectFlag "input"
                            |> Command.toCommand
                        , Command.build 123
                            |> Command.expectFlag "output"
                            |> Command.toCommand
                        ]
                in
                TypoSuggestion.getSuggestions cli "outupt"
                    |> Expect.equal
                        [ TypoSuggestion.Flag "output"
                        , TypoSuggestion.Flag "input"
                        , TypoSuggestion.Flag "unrelated"
                        ]
        ]
