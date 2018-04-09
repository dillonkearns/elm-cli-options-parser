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
                    Command.tryMatchNew [ "--help" ]
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just Help)
            , test "version command" <|
                \() ->
                    Command.tryMatchNew [ "--version" ] (Command.build Version |> Command.expectFlag "version" |> Command.toCommand)
                        |> Expect.equal (Just Version)
            , test "matching non-first element in list" <|
                \() ->
                    Command.tryMatchNew [ "unused", "--version" ] (Command.build Version |> Command.expectFlag "version" |> Command.toCommand)
                        |> Expect.equal Nothing
            , test "command with operand" <|
                \() ->
                    Command.tryMatchNew [ "http://my-domain.com" ]
                        (Command.build OpenUrl
                            |> Command.with (Command.expectOperandNew "url")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (OpenUrl "http://my-domain.com"))
            , test "command with multiple operands" <|
                \() ->
                    Command.tryMatchNew [ "http://my-domain.com", "./file.txt" ]
                        (Command.build (,)
                            |> Command.with (Command.expectOperandNew "url")
                            |> Command.with (Command.expectOperandNew "path/to/file")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just ( "http://my-domain.com", "./file.txt" ))
            , test "detects that optional flag is absent" <|
                \() ->
                    Command.tryMatchNew [ "http://my-domain.com" ]
                        (Command.build OpenUrlWithFlag
                            |> Command.with (Command.expectOperandNew "url")
                            |> Command.with (Command.withFlagNew "flag")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" False))
            , test "detects that optional flag is present" <|
                \() ->
                    Command.tryMatchNew [ "http://my-domain.com", "--flag" ]
                        (Command.build OpenUrlWithFlag
                            |> Command.with (Command.expectOperandNew "url")
                            |> Command.with (Command.withFlagNew "flag")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" True))
            , test "non-matching option" <|
                \() ->
                    Command.tryMatchNew [ "--version" ]
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "empty args when flag is expected" <|
                \() ->
                    Command.tryMatchNew []
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "option with argument" <|
                \() ->
                    Command.tryMatchNew [ "--name", "Deanna", "--prefix", "Hello" ]
                        (Command.build (,)
                            |> Command.with (Command.requiredOptionNew "name")
                            |> Command.with (Command.optionalOption "prefix")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just ( "Deanna", Just "Hello" ))
            , test "optional option with argument" <|
                \() ->
                    Command.tryMatchNew [ "--name", "Deanna" ]
                        (Command.build Name
                            |> Command.with (Command.requiredOptionNew "name")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (Name "Deanna"))
            , test "option with multiple required string arguments" <|
                \() ->
                    Command.tryMatchNew
                        [ "--last-name"
                        , "Troi"
                        , "--first-name"
                        , "Deanna"
                        ]
                        (Command.build FullName
                            |> Command.with (Command.requiredOptionNew "first-name")
                            |> Command.with (Command.requiredOptionNew "last-name")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (FullName "Deanna" "Troi"))
            , test "doesn't match if operands are present when none are expected" <|
                \() ->
                    Command.tryMatchNew
                        [ "--last-name"
                        , "Troi"
                        , "--first-name"
                        , "Deanna"
                        , "unexpectedOperand"
                        ]
                        (Command.build FullName
                            |> Command.with (Command.requiredOptionNew "first-name")
                            |> Command.with (Command.requiredOptionNew "last-name")
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "extracts multiple params" <|
                \() ->
                    Command.tryMatchNew
                        [ "--header"
                        , "abc123"
                        , "--header"
                        , "def456"
                        ]
                        (Command.build identity
                            |> Command.with (Command.optionalListOption "header")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just [ "abc123", "def456" ])
            , test "doesn't match when unexpected options are present" <|
                \() ->
                    Command.tryMatchNew
                        [ "--verbose"
                        , "--unexpected-option"
                        ]
                        (Command.build identity
                            |> Command.with (Command.withFlagNew "verbose")
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "rest operands is empty with no operands" <|
                \() ->
                    Command.tryMatchNew [ "--verbose" ]
                        (Command.build identity
                            |> Command.expectFlag "verbose"
                            |> Command.captureRestOperands "files"
                        )
                        |> Expect.equal (Just [])
            , test "rest operands has all operands when there are no required operands" <|
                \() ->
                    Command.tryMatchNew [ "--verbose", "rest1", "rest2" ]
                        (Command.build identity
                            -- TODO `expectFlag`
                            |> Command.expectFlag "verbose"
                            |> Command.captureRestOperands "files"
                        )
                        |> Expect.equal (Just [ "rest1", "rest2" ])
            , test "rest operands has all operands when there is a required operand" <|
                \() ->
                    Command.tryMatchNew [ "--something", "operand1", "rest1", "rest2" ]
                        (Command.build (,)
                            |> Command.expectFlag "something"
                            |> Command.with (Command.expectOperandNew "operand")
                            |> Command.captureRestOperands "files"
                        )
                        |> Expect.equal (Just ( "operand1", [ "rest1", "rest2" ] ))
            ]

        -- , describe "validation"
        --     [ only <|
        --         test "forced err validation makes it not match" <|
        --             \() ->
        --                 Command.tryMatchNew [ "--name", "Bob" ]
        --                     (Command.build identity
        --                         |> Command.with (Command.requiredOptionNew "name")
        --                         |> Command.toCommand
        --                     )
        --                     |> Expect.equal (Just "Bob")
        --     ]
        , describe "mapping"
            [ test "maps operand" <|
                \() ->
                    Command.tryMatchNew
                        [ "hello" ]
                        (Command.build identity
                            |> Command.with
                                (Command.expectOperandNew "operand"
                                    |> Command.mapNew String.length
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just 5)
            ]
        , describe "flags and operands extraction"
            [ test "recognizes empty operands and flags" <|
                \() ->
                    []
                        |> Command.flagsAndOperands
                            (Command.build (,)
                                |> Command.optionWithStringArg "first-name"
                                |> Command.optionWithStringArg "last-name"
                                |> Command.toCommand
                            )
                        |> expectFlagsAndOperands { flags = [], operands = [] }
            , test "gets operand from the front" <|
                \() ->
                    [ "operand", "--verbose", "--dry-run" ]
                        |> Command.flagsAndOperands
                            (Command.build (,,)
                                |> Command.expectFlag "verbose"
                                |> Command.expectFlag "dry-run"
                                |> Command.toCommand
                            )
                        |> expectFlagsAndOperands
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
                        |> expectFlagsAndOperands
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
                        |> expectFlagsAndOperands
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
                        |> expectFlagsAndOperands
                            { flags = [ "--first-name", "Will", "--last-name", "Riker" ]
                            , operands = [ "operand" ]
                            }
            , test "gets operand when there are no options" <|
                \() ->
                    [ "operand" ]
                        |> Command.flagsAndOperands
                            (Command.build identity
                                |> Command.with (Command.expectOperandNew "foo")
                                |> Command.toCommand
                            )
                        |> expectFlagsAndOperands
                            { flags = []
                            , operands = [ "operand" ]
                            }
            ]
        ]


expectFlagsAndOperands :
    { flags : flags, operands : operands }
    -> { result | flags : flags, operands : operands }
    -> Expectation
expectFlagsAndOperands expected thing =
    thing
        |> (\{ flags, operands } -> { flags = flags, operands = operands })
        |> Expect.equal expected
