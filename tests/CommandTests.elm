module CommandTests exposing (all)

import Cli.Command as Command
import Cli.Spec as Spec
import Cli.Validate as Validate
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
                    expectMatch [ "--help" ]
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
                        Help
            , test "version command" <|
                \() ->
                    expectMatch [ "--version" ]
                        (Command.build Version |> Command.expectFlag "version" |> Command.toCommand)
                        Version
            , test "matching non-first element in list" <|
                \() ->
                    expectNoMatch [ "unused", "--version" ] (Command.build Version |> Command.expectFlag "version" |> Command.toCommand)
            , test "command with operand" <|
                \() ->
                    expectMatch [ "http://my-domain.com" ]
                        (Command.build OpenUrl
                            |> Command.with (Spec.positionalArg "url")
                            |> Command.toCommand
                        )
                        (OpenUrl "http://my-domain.com")
            , test "command with multiple operands" <|
                \() ->
                    expectMatch [ "http://my-domain.com", "./file.txt" ]
                        (Command.build (,)
                            |> Command.with (Spec.positionalArg "url")
                            |> Command.with (Spec.positionalArg "path/to/file")
                            |> Command.toCommand
                        )
                        ( "http://my-domain.com", "./file.txt" )
            , test "detects that optional flag is absent" <|
                \() ->
                    expectMatch [ "http://my-domain.com" ]
                        (Command.build OpenUrlWithFlag
                            |> Command.with (Spec.positionalArg "url")
                            |> Command.with (Spec.flag "flag")
                            |> Command.toCommand
                        )
                        (OpenUrlWithFlag "http://my-domain.com" False)
            , test "detects that optional flag is present" <|
                \() ->
                    expectMatch [ "http://my-domain.com", "--flag" ]
                        (Command.build OpenUrlWithFlag
                            |> Command.with (Spec.positionalArg "url")
                            |> Command.with (Spec.flag "flag")
                            |> Command.toCommand
                        )
                        (OpenUrlWithFlag "http://my-domain.com" True)
            , test "non-matching option" <|
                \() ->
                    expectNoMatch [ "--version" ]
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
            , test "empty args when flag is expected" <|
                \() ->
                    expectNoMatch []
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.toCommand
                        )
            , test "option with argument" <|
                \() ->
                    expectMatch [ "--name", "Deanna", "--prefix", "Hello" ]
                        (Command.build (,)
                            |> Command.with (Spec.requiredKeywordArg "name")
                            |> Command.with (Spec.optionalKeywordArg "prefix")
                            |> Command.toCommand
                        )
                        ( "Deanna", Just "Hello" )
            , test "optional option with argument" <|
                \() ->
                    expectMatch [ "--name", "Deanna" ]
                        (Command.build Name
                            |> Command.with (Spec.requiredKeywordArg "name")
                            |> Command.toCommand
                        )
                        (Name "Deanna")
            , test "option with multiple required string arguments" <|
                \() ->
                    expectMatch
                        [ "--last-name"
                        , "Troi"
                        , "--first-name"
                        , "Deanna"
                        ]
                        (Command.build FullName
                            |> Command.with (Spec.requiredKeywordArg "first-name")
                            |> Command.with (Spec.requiredKeywordArg "last-name")
                            |> Command.toCommand
                        )
                        (FullName "Deanna" "Troi")
            , test "doesn't match if operands are present when none are expected" <|
                \() ->
                    expectNoMatch
                        [ "--last-name"
                        , "Troi"
                        , "--first-name"
                        , "Deanna"
                        , "unexpectedOperand"
                        ]
                        (Command.build FullName
                            |> Command.with (Spec.requiredKeywordArg "first-name")
                            |> Command.with (Spec.requiredKeywordArg "last-name")
                            |> Command.toCommand
                        )
            , test "extracts multiple params" <|
                \() ->
                    expectMatch
                        [ "--header"
                        , "abc123"
                        , "--header"
                        , "def456"
                        ]
                        (Command.build identity
                            |> Command.with (Spec.keywordArgList "header")
                            |> Command.toCommand
                        )
                        [ "abc123", "def456" ]
            , test "doesn't match when unexpected options are present" <|
                \() ->
                    expectNoMatch
                        [ "--verbose"
                        , "--unexpected-option"
                        ]
                        (Command.build identity
                            |> Command.with (Spec.flag "verbose")
                            |> Command.toCommand
                        )
            , test "rest operands is empty with no operands" <|
                \() ->
                    expectMatch [ "--verbose" ]
                        (Command.build identity
                            |> Command.expectFlag "verbose"
                            |> Command.captureRestOperands "files"
                        )
                        []
            , test "rest operands has all operands when there are no required operands" <|
                \() ->
                    expectMatch [ "--verbose", "rest1", "rest2" ]
                        (Command.build identity
                            |> Command.expectFlag "verbose"
                            |> Command.captureRestOperands "files"
                        )
                        [ "rest1", "rest2" ]
            , test "rest operands has all operands when there is a required operand" <|
                \() ->
                    expectMatch [ "--something", "operand1", "rest1", "rest2" ]
                        (Command.build (,)
                            |> Command.expectFlag "something"
                            |> Command.with (Spec.positionalArg "operand")
                            |> Command.captureRestOperands "files"
                        )
                        ( "operand1", [ "rest1", "rest2" ] )
            ]
        , describe "sub commands"
            [ test "doesn't match if sub command doesn't match" <|
                \() ->
                    expectNoMatch [ "start" ]
                        (Command.subCommand "help" identity
                            |> Command.hardcoded ()
                            |> Command.toCommand
                        )
            , test "matches if sub command is first word" <|
                \() ->
                    expectMatch [ "help" ]
                        (Command.subCommand "help" identity
                            |> Command.hardcoded ()
                            |> Command.toCommand
                        )
                        ()
            ]
        , describe "validation"
            [ test "forced err validation makes it not match" <|
                \() ->
                    Command.tryMatch [ "--name", "Bob" ]
                        (Command.build identity
                            |> Command.with
                                (Spec.requiredKeywordArg "name"
                                    |> Spec.validate (\_ -> Validate.Invalid "Invalid")
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (Err [ { name = "name", invalidReason = "Invalid", valueAsString = toString "Bob" } ]))
            , test "fails when validation function fails" <|
                \() ->
                    Command.tryMatch [ "--name", "Robert" ]
                        (Command.build identity
                            |> Command.with
                                (Spec.requiredKeywordArg "name"
                                    |> Command.validate
                                        (\name ->
                                            if String.length name == 3 then
                                                Validate.Valid
                                            else
                                                Validate.Invalid "Must be 3 characters long"
                                        )
                                )
                            |> Command.toCommand
                        )
                        |> Expect.equal (Just (Err [ { name = "name", invalidReason = "Must be 3 characters long", valueAsString = toString "Robert" } ]))
            , test "succeeds when validation function passes" <|
                \() ->
                    expectMatch [ "--name", "Bob" ]
                        (Command.build identity
                            |> Command.with
                                (Spec.requiredKeywordArg "name"
                                    |> Spec.validate
                                        (\name ->
                                            if String.length name == 3 then
                                                Validate.Valid
                                            else
                                                Validate.Invalid "Must be 3 characters long"
                                        )
                                )
                            |> Command.toCommand
                        )
                        "Bob"
            ]
        , describe "mapping"
            [ test "maps operand" <|
                \() ->
                    expectMatch
                        [ "hello" ]
                        (Command.build identity
                            |> Command.with
                                (Spec.positionalArg "operand"
                                    |> Spec.map String.length
                                )
                            |> Command.toCommand
                        )
                        5
            , test "uses default when option not present" <|
                \() ->
                    expectMatch
                        []
                        (Command.build identity
                            |> Command.with
                                (Spec.optionalKeywordArg "output"
                                    |> Spec.withDefault "elm.js"
                                )
                            |> Command.toCommand
                        )
                        "elm.js"
            , test "withDefault uses actual option when option is present" <|
                \() ->
                    expectMatch
                        [ "--output=bundle.js" ]
                        (Command.build identity
                            |> Command.with
                                (Spec.optionalKeywordArg "output"
                                    |> Spec.withDefault "elm.js"
                                )
                            |> Command.toCommand
                        )
                        "bundle.js"
            , test "hardcoded passes value through" <|
                \() ->
                    expectMatch
                        []
                        (Command.build identity
                            |> Command.hardcoded "hardcoded value"
                            |> Command.toCommand
                        )
                        "hardcoded value"
            ]
        ]


expectMatch : List String -> Command.Command a -> a -> Expectation
expectMatch argv commands expectedValue =
    Command.tryMatch argv commands
        |> Expect.equal (Just (Ok expectedValue))


expectNoMatch : List String -> Command.Command a -> Expectation
expectNoMatch argv commands =
    Command.tryMatch argv commands
        |> Expect.equal Nothing
