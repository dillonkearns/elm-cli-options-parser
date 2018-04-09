module Cli.UsageSpecTests exposing (all)

import Command
import Expect exposing (Expectation)
import Test exposing (..)


all : Test
all =
    describe "synopsis"
        [ test "synopsis prints options with arguments" <|
            \() ->
                (Command.build (,)
                    |> Command.optionWithStringArg "first-name"
                    |> Command.optionWithStringArg "last-name"
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
                    |> Command.optionWithStringArg "name"
                    |> Command.optionalOptionWithStringArg "prefix"
                    |> Command.toCommand
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --name <name> [--prefix <prefix>]"
        , test "print synopsis with required operand" <|
            \() ->
                Command.build identity
                    |> Command.expectOperand "MyApp.elm"
                    |> Command.toCommand
                    |> Command.synopsis "elm-interop"
                    |> Expect.equal "elm-interop <MyApp.elm>"
        , test "print synopsis with doc string" <|
            \() ->
                Command.buildWithDoc (,) "greets somebody in your terminal"
                    |> Command.optionWithStringArg "name"
                    |> Command.optionalOptionWithStringArg "prefix"
                    |> Command.toCommand
                    |> Command.synopsis "greet"
                    |> Expect.equal "greet --name <name> [--prefix <prefix>] # greets somebody in your terminal"
        , test "print synopsis with zero or more arg option" <|
            \() ->
                (Command.build identity
                    |> Command.with (Command.optionalListOption "header")
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
                    |> Command.with (Command.withFlagNew "dry-run")
                    |> Command.captureRestOperands "files"
                    |> Command.synopsis "rm"
                    |> Expect.equal "rm [--dry-run] <files>..."
        ]
