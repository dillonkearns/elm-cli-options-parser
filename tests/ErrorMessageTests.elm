module ErrorMessageTests exposing (all)

{-| Tests for user-facing error messages.

The goal is to ensure error messages are:

1.  Specific about what went wrong
2.  Actionable (tell user how to fix it)
3.  User-friendly (no internal jargon like "optionsParser")

-}

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.MatchResult exposing (MatchResult(..), NoMatchReason(..))
import Expect exposing (Expectation)
import Test exposing (..)


all : Test
all =
    describe "Error messages - structured NoMatchReason"
        [ describe "subcommand errors"
            [ test "missing subcommand returns MissingSubCommand reason" <|
                \() ->
                    -- User runs: program (with no args)
                    -- Parser expects: program init
                    let
                        parser =
                            OptionsParser.buildSubCommand "init" "init"
                                |> OptionsParser.end

                        argv =
                            []
                    in
                    OptionsParser.tryMatch argv parser
                        |> Expect.equal
                            (NoMatch [ MissingSubCommand { expectedSubCommand = "init" } ])
            , test "wrong subcommand returns WrongSubCommand reason" <|
                \() ->
                    -- User runs: program foo
                    -- Parser expects: program init
                    let
                        parser =
                            OptionsParser.buildSubCommand "init" "init"
                                |> OptionsParser.end

                        argv =
                            [ "foo" ]
                    in
                    OptionsParser.tryMatch argv parser
                        |> Expect.equal
                            (NoMatch
                                [ WrongSubCommand
                                    { expectedSubCommand = "init"
                                    , actualSubCommand = "foo"
                                    }
                                ]
                            )
            ]
        , describe "missing required arguments"
            [ test "missing required positional arg returns structured reason" <|
                \() ->
                    -- User runs: program clone (missing <url>)
                    let
                        parser =
                            OptionsParser.buildSubCommand "clone" identity
                                |> OptionsParser.with (Option.requiredPositionalArg "url")
                                |> OptionsParser.end

                        argv =
                            [ "clone" ]
                    in
                    OptionsParser.tryMatch argv parser
                        |> Expect.equal
                            (NoMatch [ MissingRequiredPositionalArg { name = "url" } ])
            , test "missing required keyword arg returns structured reason" <|
                \() ->
                    -- User runs: program (without --name)
                    let
                        parser =
                            OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                                |> OptionsParser.end

                        argv =
                            []
                    in
                    OptionsParser.tryMatch argv parser
                        |> Expect.equal
                            (NoMatch [ MissingRequiredKeywordArg { name = "name" } ])
            ]
        , describe "expected flag not present"
            [ test "missing expected flag returns structured reason" <|
                \() ->
                    -- User runs: program (without --verbose)
                    -- Parser expects --verbose flag via expectFlag
                    let
                        parser =
                            OptionsParser.build "matched"
                                |> OptionsParser.expectFlag "verbose"
                                |> OptionsParser.end

                        argv =
                            []
                    in
                    OptionsParser.tryMatch argv parser
                        |> Expect.equal
                            (NoMatch [ MissingExpectedFlag { name = "verbose" } ])
            ]
        , describe "extra operands"
            [ test "extra positional args returns ExtraOperand reason" <|
                \() ->
                    -- User runs: program init extra-arg
                    -- Parser only expects: program init
                    let
                        parser =
                            OptionsParser.buildSubCommand "init" "init"
                                |> OptionsParser.end

                        argv =
                            [ "init", "extra-arg" ]
                    in
                    OptionsParser.tryMatch argv parser
                        |> Expect.equal (NoMatch [ ExtraOperand ])
            ]
        , describe "unexpected options"
            [ test "unknown flag returns UnexpectedOption reason" <|
                \() ->
                    -- User runs: program --unknown
                    let
                        parser =
                            OptionsParser.build identity
                                |> OptionsParser.with (Option.flag "verbose")
                                |> OptionsParser.end

                        argv =
                            [ "--unknown" ]
                    in
                    OptionsParser.tryMatch argv parser
                        |> Expect.equal (NoMatch [ UnexpectedOption "unknown" ])
            , test "multiple unknown flags returns multiple UnexpectedOption reasons" <|
                \() ->
                    let
                        parser =
                            OptionsParser.build identity
                                |> OptionsParser.with (Option.flag "verbose")
                                |> OptionsParser.end

                        argv =
                            [ "--foo", "--bar" ]
                    in
                    OptionsParser.tryMatch argv parser
                        |> Expect.equal
                            (NoMatch
                                [ UnexpectedOption "foo"
                                , UnexpectedOption "bar"
                                ]
                            )
            ]
        ]
