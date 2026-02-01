module TypoSuggestionTests exposing (all)

import Cli.ColorMode exposing (ColorMode(..))
import Cli.UsageSpec as UsageSpec
import Expect exposing (Expectation)
import Occurences exposing (Occurences(..))
import Test exposing (..)
import TypoSuggestion


all : Test
all =
    describe "typo suggestions"
        [ describe "ordering"
            [ test "exact matching suboptionsParser" <|
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
                            [ requiredFlagOptionsParser "unrelated"
                            , requiredFlagOptionsParser "input"
                            , requiredFlagOptionsParser "output"
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
                        [ requiredFlagOptionsParser "unrelated"
                        , requiredFlagOptionsParser "input"
                        , requiredFlagOptionsParser "output"
                        ]
                in
                TypoSuggestion.toMessage WithoutColor cli "outupt"
                    |> Expect.equal "The `--outupt` flag was not found. Maybe it was one of these typos?\n\n`--outupt` <> `--output`"
        ]


requiredFlagOptionsParser : String -> TypoSuggestion.OptionsParser
requiredFlagOptionsParser flagName =
    { subCommand = Nothing
    , usageSpecs = [ UsageSpec.flag flagName Required ]
    }


subCommand : String -> TypoSuggestion.OptionsParser
subCommand subCommandName =
    { subCommand = Just subCommandName
    , usageSpecs = []
    }
