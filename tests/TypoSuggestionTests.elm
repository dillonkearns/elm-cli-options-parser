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
                        [ Command.build 123
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        , Command.subCommand "sub" 456
                            |> Command.toCommand
                        ]
                in
                TypoSuggestion.getSuggestions cli "sub"
                    |> Expect.equal [ TypoSuggestion.SubCommand "sub" ]
        ]
