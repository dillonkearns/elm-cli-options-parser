module Cli.UsageSpecTests exposing (all)

import Cli.Command as Command
import Cli.Spec as Spec
import Expect exposing (Expectation)
import Test exposing (..)


all : Test
all =
    describe "synopsis"
        [ test "synopsis prints options with arguments" <|
            \() ->
                (Command.build (,)
                    |> Command.with (Command.requiredKeywordArg "first-name")
                    |> Command.with (Command.requiredKeywordArg "last-name")
                    |> Command.toCommand
                )
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --first-name <first-name> --last-name <last-name>"
        , test "print synopsis with required flag" <|
            \() ->
                Command.build (,)
                    |> Command.expectFlag "version"
                    |> Command.toCommand
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --version"
        , test "print synopsis with optional arg" <|
            \() ->
                Command.build (,)
                    |> Command.with (Command.requiredKeywordArg "name")
                    |> Command.with (Command.optionalKeywordArg "prefix")
                    |> Command.toCommand
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --name <name> [--prefix <prefix>]"
        , test "print synopsis with required operand" <|
            \() ->
                Command.build identity
                    |> Command.with (Command.positionalArg "MyApp.elm")
                    |> Command.toCommand
                    |> Command.synopsis "elm-interop"
                    |> Expect.equal "elm-interop <MyApp.elm>"
        , test "print synopsis with doc string" <|
            \() ->
                Command.buildWithDoc (,) "greets somebody in your terminal"
                    |> Command.with (Command.requiredKeywordArg "name")
                    |> Command.with (Command.optionalKeywordArg "prefix")
                    |> Command.toCommand
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --name <name> [--prefix <prefix>] # greets somebody in your terminal"
        , test "print synopsis with zero or more arg option" <|
            \() ->
                (Command.build identity
                    |> Command.with (Command.keywordArgList "header")
                )
                    |> Command.toCommand
                    |> Command.synopsis "curl"
                    |> Expect.equal "curl [--header <header>]..."
        , test "print rest operands synopsis" <|
            \() ->
                Command.build identity
                    |> Command.captureRestOperands "files"
                    |> Command.synopsis "rm"
                    |> Expect.equal "rm <files>..."
        , test "prints rest args at the end of the synopsis" <|
            \() ->
                Command.build (,)
                    |> Command.with (Command.flag "dry-run")
                    |> Command.captureRestOperands "files"
                    |> Command.synopsis "rm"
                    |> Expect.equal "rm [--dry-run] <files>..."
        , test "shows sub commands" <|
            \() ->
                Command.subCommand "init" identity
                    |> Command.hardcoded ()
                    |> Command.toCommand
                    |> Command.synopsis "elm-test"
                    |> Expect.equal "elm-test init"
        , test "mutually exclusive keyword arg" <|
            \() ->
                Command.build identity
                    |> Command.with
                        (Spec.requiredKeywordArg "report"
                            |> Spec.oneOf 123
                                [ Spec.Thing "json" 123
                                , Spec.Thing "junit" 123
                                , Spec.Thing "console" 123
                                ]
                        )
                    |> Command.toCommand
                    |> Command.synopsis "elm-test"
                    |> Expect.equal "elm-test --report <json|junit|console>"
        , test "sub-command with flag" <|
            \() ->
                Command.subCommand "log" identity
                    |> Command.with (Spec.flag "stat")
                    |> Command.toCommand
                    |> Command.synopsis "git"
                    |> Expect.equal "git log [--stat]"
        ]
