module TypoSuggestionTests exposing (all)

import Cli.UsageSpec as UsageSpec
import Expect exposing (Expectation)
import Occurences exposing (Occurences(Required))
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
                            [ subCommand "sub"
                            ]
                    in
                    TypoSuggestion.getSuggestions cli "sub"
                        |> Expect.equal [ TypoSuggestion.SubCommand "sub" ]
            , test "letter swapped in flag name" <|
                \() ->
                    let
                        cli =
                            [ requiredFlagCommand "unrelated"
                            , requiredFlagCommand "input"
                            , requiredFlagCommand "output"
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
                        [ requiredFlagCommand "unrelated"
                        , requiredFlagCommand "input"
                        , requiredFlagCommand "output"
                        ]
                in
                TypoSuggestion.toMessage cli "outupt"
                    |> Expect.equal "The `--outupt` flag was not found. Maybe it was one of these typos?\n\n`--outupt` <> `--output`"
        ]


requiredFlagCommand : String -> TypoSuggestion.Command
requiredFlagCommand flagName =
    { subCommand = Nothing
    , usageSpecs = [ UsageSpec.flag flagName Required ]
    }


subCommand : String -> TypoSuggestion.Command
subCommand subCommandName =
    { subCommand = Just subCommandName
    , usageSpecs = []
    }
