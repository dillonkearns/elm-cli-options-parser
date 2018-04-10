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
                    Command.tryMatch [ "--help" ]
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
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
                    Command.tryMatch [ "http://my-domain.com" ]
                        (Command.build OpenUrl
                            |> Command.with (Command.requiredOperand "url")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (OpenUrl "http://my-domain.com"))
            , test "command with multiple operands" <|
                \() ->
                    Command.tryMatch [ "http://my-domain.com", "./file.txt" ]
                        (Command.build (,)
                            |> Command.with (Command.requiredOperand "url")
                            |> Command.with (Command.requiredOperand "path/to/file")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just ( "http://my-domain.com", "./file.txt" ))
            , test "detects that optional flag is absent" <|
                \() ->
                    Command.tryMatch [ "http://my-domain.com" ]
                        (Command.build OpenUrlWithFlag
                            |> Command.with (Command.requiredOperand "url")
                            |> Command.with (Command.optionalFlag "flag")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" False))
            , test "detects that optional flag is present" <|
                \() ->
                    Command.tryMatch [ "http://my-domain.com", "--flag" ]
                        (Command.build OpenUrlWithFlag
                            |> Command.with (Command.requiredOperand "url")
                            |> Command.with (Command.optionalFlag "flag")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (OpenUrlWithFlag "http://my-domain.com" True))
            , test "non-matching option" <|
                \() ->
                    Command.tryMatch [ "--version" ]
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "empty args when flag is expected" <|
                \() ->
                    Command.tryMatch []
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "option with argument" <|
                \() ->
                    Command.tryMatch [ "--name", "Deanna", "--prefix", "Hello" ]
                        (Command.build (,)
                            |> Command.with (Command.requiredOption "name")
                            |> Command.with (Command.optionalOption "prefix")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just ( "Deanna", Just "Hello" ))
            , test "optional option with argument" <|
                \() ->
                    Command.tryMatch [ "--name", "Deanna" ]
                        (Command.build Name
                            |> Command.with (Command.requiredOption "name")
                            |> Command.toCommand
                        )
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
                            |> Command.with (Command.requiredOption "first-name")
                            |> Command.with (Command.requiredOption "last-name")
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
                            |> Command.with (Command.requiredOption "first-name")
                            |> Command.with (Command.requiredOption "last-name")
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
                            |> Command.with (Command.optionalListOption "header")
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just [ "abc123", "def456" ])
            , test "doesn't match when unexpected options are present" <|
                \() ->
                    Command.tryMatch
                        [ "--verbose"
                        , "--unexpected-option"
                        ]
                        (Command.build identity
                            |> Command.with (Command.optionalFlag "verbose")
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
                            |> Command.with (Command.requiredOperand "operand")
                            |> Command.captureRestOperands "files"
                        )
                        |> Expect.equal (Just ( "operand1", [ "rest1", "rest2" ] ))
            ]
        , describe "validation"
            [ test "forced err validation makes it not match" <|
                \() ->
                    Command.tryMatch [ "--name", "Bob" ]
                        (Command.build identity
                            |> Command.with
                                (Command.requiredOption "name"
                                    |> Command.validate (\_ -> Command.Invalid "Invalid")
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "fails when validation function fails" <|
                \() ->
                    Command.tryMatch [ "--name", "Robert" ]
                        (Command.build identity
                            |> Command.with
                                (Command.requiredOption "name"
                                    |> Command.validate
                                        (\name ->
                                            if String.length name == 3 then
                                                Command.Valid
                                            else
                                                Command.Invalid "Must be 3 characters long"
                                        )
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal Nothing
            , test "succeeds when validation function passes" <|
                \() ->
                    Command.tryMatch [ "--name", "Bob" ]
                        (Command.build identity
                            |> Command.with
                                (Command.requiredOption "name"
                                    |> Command.validate
                                        (\name ->
                                            if String.length name == 3 then
                                                Command.Valid
                                            else
                                                Command.Invalid "Must be 3 characters long"
                                        )
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just "Bob")
            ]
        , describe "mapping"
            [ test "maps operand" <|
                \() ->
                    Command.tryMatch
                        [ "hello" ]
                        (Command.build identity
                            |> Command.with
                                (Command.requiredOperand "operand"
                                    |> Command.mapNew String.length
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just 5)
            , test "uses default when option not present" <|
                \() ->
                    Command.tryMatch
                        []
                        (Command.build identity
                            |> Command.with
                                (Command.optionalOption "output"
                                    |> Command.withDefault "elm.js"
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just "elm.js")
            , test "withDefault uses actual option when option is present" <|
                \() ->
                    Command.tryMatch
                        [ "--output=bundle.js" ]
                        (Command.build identity
                            |> Command.with
                                (Command.optionalOption "output"
                                    |> Command.withDefault "elm.js"
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just "bundle.js")
            , test "hardcoded passes value through" <|
                \() ->
                    Command.tryMatch
                        []
                        (Command.build identity
                            |> Command.hardcoded "hardcoded value"
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just "hardcoded value")
            ]
        ]
