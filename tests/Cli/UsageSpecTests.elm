module Cli.UsageSpecTests exposing (all)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Expect exposing (Expectation)
import Test exposing (..)


all : Test
all =
    describe "synopsis"
        [ test "synopsis prints options with arguments" <|
            \() ->
                (OptionsParser.build (\a b -> ( a, b ))
                    |> OptionsParser.with (Option.requiredKeywordArg "first-name")
                    |> OptionsParser.with (Option.requiredKeywordArg "last-name")
                    |> OptionsParser.end
                )
                    |> OptionsParser.synopsis "greet"
                    |> Expect.equal "greet --first-name <first-name> --last-name <last-name>"
        , test "print synopsis with required flag" <|
            \() ->
                OptionsParser.build (\a b -> ( a, b ))
                    |> OptionsParser.expectFlag "version"
                    |> OptionsParser.end
                    |> OptionsParser.synopsis "greet"
                    |> Expect.equal "greet --version"
        , test "print synopsis with optional arg" <|
            \() ->
                OptionsParser.build (\a b -> ( a, b ))
                    |> OptionsParser.with (Option.requiredKeywordArg "name")
                    |> OptionsParser.with (Option.optionalKeywordArg "prefix")
                    |> OptionsParser.end
                    |> OptionsParser.synopsis "greet"
                    |> Expect.equal "greet --name <name> [--prefix <prefix>]"
        , test "print synopsis with required operand" <|
            \() ->
                OptionsParser.build identity
                    |> OptionsParser.with (Option.requiredPositionalArg "MyApp.elm")
                    |> OptionsParser.end
                    |> OptionsParser.synopsis "elm-interop"
                    |> Expect.equal "elm-interop <MyApp.elm>"
        , test "synopsis for optional positional argument" <|
            \() ->
                OptionsParser.build identity
                    |> OptionsParser.withOptionalPositionalArg (Option.optionalPositionalArg "revision range")
                    |> OptionsParser.synopsis "git"
                    |> Expect.equal "git [<revision range>]"
        , test "print synopsis with doc string" <|
            \() ->
                OptionsParser.build (\a b -> ( a, b ))
                    |> OptionsParser.with (Option.requiredKeywordArg "name")
                    |> OptionsParser.with (Option.optionalKeywordArg "prefix")
                    |> OptionsParser.end
                    |> OptionsParser.withDoc "greets somebody in your terminal"
                    |> OptionsParser.synopsis "greet"
                    |> Expect.equal "greet --name <name> [--prefix <prefix>] # greets somebody in your terminal"
        , test "print synopsis with zero or more arg option" <|
            \() ->
                (OptionsParser.build identity
                    |> OptionsParser.with (Option.keywordArgList "header")
                )
                    |> OptionsParser.end
                    |> OptionsParser.synopsis "curl"
                    |> Expect.equal "curl [--header <header>]..."
        , test "print rest operands synopsis" <|
            \() ->
                OptionsParser.build identity
                    |> OptionsParser.withRestArgs (Option.restArgs "files")
                    |> OptionsParser.synopsis "rm"
                    |> Expect.equal "rm <files>..."
        , test "prints rest args at the end of the synopsis" <|
            \() ->
                OptionsParser.build (\a b -> ( a, b ))
                    |> OptionsParser.with (Option.flag "dry-run")
                    |> OptionsParser.withRestArgs (Option.restArgs "files")
                    |> OptionsParser.synopsis "rm"
                    |> Expect.equal "rm [--dry-run] <files>..."
        , test "shows sub optionsParsers" <|
            \() ->
                OptionsParser.buildSubCommand "init" identity
                    |> OptionsParser.end
                    |> OptionsParser.synopsis "elm-test"
                    |> Expect.equal "elm-test init"
        , test "mutually exclusive keyword arg" <|
            \() ->
                OptionsParser.build identity
                    |> OptionsParser.with
                        (Option.requiredKeywordArg "report"
                            |> Option.oneOf 123
                                [ ( "json", 123 )
                                , ( "junit", 123 )
                                , ( "console", 123 )
                                ]
                        )
                    |> OptionsParser.end
                    |> OptionsParser.synopsis "elm-test"
                    |> Expect.equal "elm-test --report <json|junit|console>"
        , test "mutually exclusive positional arg" <|
            \() ->
                OptionsParser.build identity
                    |> OptionsParser.with
                        (Option.requiredPositionalArg "report"
                            |> Option.oneOf 123
                                [ ( "json", 123 )
                                , ( "junit", 123 )
                                , ( "console", 123 )
                                ]
                        )
                    |> OptionsParser.end
                    |> OptionsParser.synopsis "elm-test"
                    |> Expect.equal "elm-test <json|junit|console>"
        , test "sub-optionsParser with flag" <|
            \() ->
                OptionsParser.buildSubCommand "log" identity
                    |> OptionsParser.with (Option.flag "stat")
                    |> OptionsParser.end
                    |> OptionsParser.synopsis "git"
                    |> Expect.equal "git log [--stat]"
        ]
