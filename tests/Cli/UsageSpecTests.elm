module Cli.UsageSpecTests exposing (all)

import Cli.Command as Command
import Cli.Option as Option
import Expect exposing (Expectation)
import Test exposing (..)


all : Test
all =
    describe "synopsis"
        [ test "synopsis prints options with arguments" <|
            \() ->
                (Command.build (,)
                    |> Command.with (Option.requiredKeywordArg "first-name")
                    |> Command.with (Option.requiredKeywordArg "last-name")
                    |> Command.withoutRestArgs
                )
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --first-name <first-name> --last-name <last-name>"
        , test "print synopsis with required flag" <|
            \() ->
                Command.build (,)
                    |> Command.expectFlag "version"
                    |> Command.withoutRestArgs
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --version"
        , test "print synopsis with optional arg" <|
            \() ->
                Command.build (,)
                    |> Command.with (Option.requiredKeywordArg "name")
                    |> Command.with (Option.optionalKeywordArg "prefix")
                    |> Command.withoutRestArgs
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --name <name> [--prefix <prefix>]"
        , test "print synopsis with required operand" <|
            \() ->
                Command.build identity
                    |> Command.with (Option.positionalArg "MyApp.elm")
                    |> Command.withoutRestArgs
                    |> Command.synopsis "elm-interop"
                    |> Expect.equal "elm-interop <MyApp.elm>"
        , test "print synopsis with doc string" <|
            \() ->
                Command.build (,)
                    |> Command.with (Option.requiredKeywordArg "name")
                    |> Command.with (Option.optionalKeywordArg "prefix")
                    |> Command.withoutRestArgs
                    |> Command.withDoc "greets somebody in your terminal"
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --name <name> [--prefix <prefix>] # greets somebody in your terminal"
        , test "print synopsis with zero or more arg option" <|
            \() ->
                (Command.build identity
                    |> Command.with (Option.keywordArgList "header")
                )
                    |> Command.withoutRestArgs
                    |> Command.synopsis "curl"
                    |> Expect.equal "curl [--header <header>]..."
        , test "print rest operands synopsis" <|
            \() ->
                Command.build identity
                    |> Command.withRestArgs "files"
                    |> Command.synopsis "rm"
                    |> Expect.equal "rm <files>..."
        , test "prints rest args at the end of the synopsis" <|
            \() ->
                Command.build (,)
                    |> Command.with (Option.flag "dry-run")
                    |> Command.withRestArgs "files"
                    |> Command.synopsis "rm"
                    |> Expect.equal "rm [--dry-run] <files>..."
        , test "shows sub commands" <|
            \() ->
                Command.buildSubCommand "init" identity
                    |> Command.withoutRestArgs
                    |> Command.synopsis "elm-test"
                    |> Expect.equal "elm-test init"
        , test "mutually exclusive keyword arg" <|
            \() ->
                Command.build identity
                    |> Command.with
                        (Option.requiredKeywordArg "report"
                            |> Option.oneOf 123
                                [ Option.MutuallyExclusiveValue "json" 123
                                , Option.MutuallyExclusiveValue "junit" 123
                                , Option.MutuallyExclusiveValue "console" 123
                                ]
                        )
                    |> Command.withoutRestArgs
                    |> Command.synopsis "elm-test"
                    |> Expect.equal "elm-test --report <json|junit|console>"
        , test "mutually exclusive positional arg" <|
            \() ->
                Command.build identity
                    |> Command.with
                        (Option.positionalArg "report"
                            |> Option.oneOf 123
                                [ Option.MutuallyExclusiveValue "json" 123
                                , Option.MutuallyExclusiveValue "junit" 123
                                , Option.MutuallyExclusiveValue "console" 123
                                ]
                        )
                    |> Command.withoutRestArgs
                    |> Command.synopsis "elm-test"
                    |> Expect.equal "elm-test <json|junit|console>"
        , test "sub-command with flag" <|
            \() ->
                Command.buildSubCommand "log" identity
                    |> Command.with (Option.flag "stat")
                    |> Command.withoutRestArgs
                    |> Command.synopsis "git"
                    |> Expect.equal "git log [--stat]"
        ]
