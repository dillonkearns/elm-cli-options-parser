module CommandTests exposing (all)

import Command
import Expect exposing (Expectation)
import Test exposing (..)


type Msg
    = Help
    | Version
    | OpenUrl String
    | OpenUrlWithFlag String Bool
    | Name String
    | FullName String String


all : Test
all =
    describe "CLI options parser"
        [ describe "matching"
            [ test "help command" <|
                \() ->
                    Command.tryMatch [ "--help" ] (Command.build Help |> Command.expectFlag "help" |> Command.toCommand)
                        |> Expect.equal (Just Help)
            , test "version command" <|
                \() ->
                    Command.tryMatch [ "--version" ] (Command.build Version |> Command.expectFlag "version" |> Command.toCommand)
                        |> Expect.equal (Just Version)
            , test "matching non-first element in list" <|
                \() ->
                    Command.tryMatch [ "unused", "--version" ] (Command.build Version |> Command.expectFlag "version" |> Command.toCommand)
                        |> Expect.equal Nothing
            , test "command with operand" <|
                \() ->
                    Command.tryMatch [ "http://my-domain.com" ] (Command.build OpenUrl |> Command.expectOperand "url" |> Command.toCommand)
                        |> Expect.equal (Just (OpenUrl "http://my-domain.com"))
            , test "command with multiple operands" <|
                \() ->
                    Command.tryMatch [ "http://my-domain.com", "./file.txt" ]
                        (Command.build (,)
                            |> Command.expectOperand "url"
                            |> Command.expectOperand "path/to/file"
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just ( "http://my-domain.com", "./file.txt" ))
            , test "detects that optional flag is absent" <|
                \() ->
                    Command.tryMatch [ "http://my-domain.com" ] (Command.build OpenUrlWithFlag |> Command.expectOperand "url" |> Command.withFlag "p" |> Command.toCommand)
                        |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" False))
            , test "detects that optional flag is present" <|
                \() ->
                    Command.tryMatch [ "http://my-domain.com", "--p" ] (Command.build OpenUrlWithFlag |> Command.expectOperand "url" |> Command.withFlag "p" |> Command.toCommand)
                        |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" True))
            , test "non-matching option" <|
                \() ->
                    Command.tryMatch [ "--version" ] (Command.build Help |> Command.expectFlag "help" |> Command.toCommand)
                        |> Expect.equal Nothing
            , test "option with argument" <|
                \() ->
                    Command.tryMatch [ "--name", "Deanna", "--prefix", "Hello" ]
                        (Command.build (,)
                            |> Command.optionWithStringArg "name"
                            |> Command.optionalOptionWithStringArg "prefix"
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just ( "Deanna", Just "Hello" ))
            , test "optional option with argument" <|
                \() ->
                    Command.tryMatch [ "--name", "Deanna" ] (Command.build Name |> Command.optionWithStringArg "name" |> Command.toCommand)
                        |> Expect.equal (Just (Name "Deanna"))
            , test "option with multiple required string arguments" <|
                \() ->
                    Command.tryMatch
                        [ "--last-name"
                        , "Troi"
                        , "--first-name"
                        , "Deanna"
                        ]
                        (Command.build FullName
                            |> Command.optionWithStringArg "first-name"
                            |> Command.optionWithStringArg "last-name"
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (FullName "Deanna" "Troi"))
            , test "doesn't match if operands are present when none are expected" <|
                \() ->
                    Command.tryMatch
                        [ "--last-name"
                        , "Troi"
                        , "--first-name"
                        , "Deanna"
                        , "unexpectedOperand"
                        ]
                        (Command.build FullName
                            |> Command.optionWithStringArg "first-name"
                            |> Command.optionWithStringArg "last-name"
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "extracts multiple params" <|
                \() ->
                    Command.tryMatch
                        [ "--header"
                        , "abc123"
                        , "--header"
                        , "def456"
                        ]
                        (Command.build identity
                            |> Command.zeroOrMoreWithStringArg "header"
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just [ "abc123", "def456" ])
            , test "doesn't match when unexpected options are present" <|
                \() ->
                    Command.tryMatch
                        [ "--verbose"
                        , "--unexpected-option"
                        ]
                        (Command.build FullName
                            |> Command.expectFlag "verbose"
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "rest operands is empty with no operands" <|
                \() ->
                    Command.tryMatch [ "--verbose" ]
                        (Command.build identity
                            |> Command.expectFlag "verbose"
                            |> Command.captureRestOperands "files"
                        )
                        |> Expect.equal (Just [])
            , test "rest operands has all operands when there are no required operands" <|
                \() ->
                    Command.tryMatch [ "--verbose", "rest1", "rest2" ]
                        (Command.build identity
                            |> Command.expectFlag "verbose"
                            |> Command.captureRestOperands "files"
                        )
                        |> Expect.equal (Just [ "rest1", "rest2" ])
            , test "rest operands has all operands when there is a required operand" <|
                \() ->
                    Command.tryMatch [ "--something", "operand1", "rest1", "rest2" ]
                        (Command.build (,)
                            |> Command.expectFlag "something"
                            |> Command.expectOperand "operand"
                            |> Command.captureRestOperands "files"
                        )
                        |> Expect.equal (Just ( "operand1", [ "rest1", "rest2" ] ))
            ]
        , describe "flags and operands extraction"
            [ test "recognizes empty operands and flags" <|
                \() ->
                    []
                        |> Command.flagsAndOperands
                            (Command.build FullName
                                |> Command.optionWithStringArg "first-name"
                                |> Command.optionWithStringArg "last-name"
                                |> Command.toCommand
                            )
                        |> Expect.equal { flags = [], operands = [] }
            , test "gets operand from the front" <|
                \() ->
                    [ "operand", "--verbose", "--dry-run" ]
                        |> Command.flagsAndOperands
                            (Command.build (,,)
                                |> Command.expectFlag "verbose"
                                |> Command.expectFlag "dry-run"
                                |> Command.toCommand
                            )
                        |> Expect.equal
                            { flags = [ "--verbose", "--dry-run" ]
                            , operands = [ "operand" ]
                            }
            , test "gets operand from the back" <|
                \() ->
                    [ "--verbose", "--dry-run", "operand" ]
                        |> Command.flagsAndOperands
                            (Command.build (,,)
                                |> Command.expectFlag "verbose"
                                |> Command.expectFlag "dry-run"
                                |> Command.toCommand
                            )
                        |> Expect.equal
                            { flags = [ "--verbose", "--dry-run" ]
                            , operands = [ "operand" ]
                            }
            , test "gets operand from the front when args are used" <|
                \() ->
                    [ "operand", "--first-name", "Will", "--last-name", "Riker" ]
                        |> Command.flagsAndOperands
                            (Command.build FullName
                                |> Command.optionWithStringArg "first-name"
                                |> Command.optionWithStringArg "last-name"
                                |> Command.toCommand
                            )
                        |> Expect.equal
                            { flags = [ "--first-name", "Will", "--last-name", "Riker" ]
                            , operands = [ "operand" ]
                            }
            , test "gets operand from the back when args are present" <|
                \() ->
                    [ "--first-name", "Will", "--last-name", "Riker", "operand" ]
                        |> Command.flagsAndOperands
                            (Command.build FullName
                                |> Command.optionWithStringArg "first-name"
                                |> Command.optionWithStringArg "last-name"
                                |> Command.toCommand
                            )
                        |> Expect.equal
                            { flags = [ "--first-name", "Will", "--last-name", "Riker" ]
                            , operands = [ "operand" ]
                            }
            , test "gets operand when there are no options" <|
                \() ->
                    [ "operand" ]
                        |> Command.flagsAndOperands
                            (Command.build identity
                                |> Command.expectOperand "foo"
                                |> Command.toCommand
                            )
                        |> Expect.equal
                            { flags = []
                            , operands = [ "operand" ]
                            }
            ]
        , describe "synopsis"
            [ test "synopsis prints options with arguments" <|
                \() ->
                    (Command.build FullName
                        |> Command.optionWithStringArg "first-name"
                        |> Command.optionWithStringArg "last-name"
                        |> Command.toCommand
                    )
                        |> Command.synopsis "greet"
                        |> Expect.equal "greet --first-name <first-name> --last-name <last-name>"
            , test "print synopsis with required flag" <|
                \() ->
                    Command.build Version
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
                        |> Command.zeroOrMoreWithStringArg "header"
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
                        |> Command.withFlag "dry-run"
                        |> Command.captureRestOperands "files"
                        |> Command.synopsis "rm"
                        |> Expect.equal "rm [--dry-run] <files>..."
            ]
        ]



-- |> Expect.equal "greet -n <name> [-l][-a][-c option_argument][operand...]"
