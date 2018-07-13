module TypoSuggestionTests exposing (all)

import Cli.Command as Command
import Expect exposing (Expectation)
import Test exposing (..)
import TypoSuggestion


all : Test
all =
    describe "typo suggestions"
        [ describe "ordering"
            [ test "exact matching subcommand" <|
                \() ->
                    let
                        cli =
                            [ Command.subCommand "sub" 456
                                |> Command.withoutRestArgs
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
                                |> Command.withoutRestArgs
                            , Command.build 123
                                |> Command.expectFlag "input"
                                |> Command.withoutRestArgs
                            , Command.build 123
                                |> Command.expectFlag "output"
                                |> Command.withoutRestArgs
                            ]
                    in
                    TypoSuggestion.getSuggestions cli "outupt"
                        |> Expect.equal
                            [ TypoSuggestion.Flag "output"
                            , TypoSuggestion.Flag "input"
                            , TypoSuggestion.Flag "unrelated"
                            ]
            ]
        , test "message text" <|
            \() ->
                let
                    cli =
                        [ Command.build 123
                            |> Command.expectFlag "unrelated"
                            |> Command.withoutRestArgs
                        , Command.build 123
                            |> Command.expectFlag "input"
                            |> Command.withoutRestArgs
                        , Command.build 123
                            |> Command.expectFlag "output"
                            |> Command.withoutRestArgs
                        ]
                in
                TypoSuggestion.toMessage cli "outupt"
                    |> Expect.equal "The `--outupt` flag was not found. Maybe it was one of these typos?\n\n`--outupt` <> `--output`"
        ]
