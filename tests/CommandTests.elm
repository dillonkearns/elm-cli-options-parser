module CommandTests exposing (all)

import Cli.Command as Command
import Cli.Command.MatchResult
import Cli.Option as Option
import Cli.Validate as Validate
import Expect exposing (Expectation)
import Test exposing (..)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type WatchMode
    = Watch
    | NoWatch


type Msg
    = Help
    | Version
    | OpenUrl String
    | OpenUrlWithFlag String Bool
    | Name String
    | FullName String String


type Report
    = Console
    | Json
    | Junit


all : Test
all =
    describe "CLI options parser"
        [ describe "matching"
            [ test "help command" <|
                \() ->
                    expectMatch [ "--help" ]
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.end
                        )
                        Help
            , test "version command" <|
                \() ->
                    expectMatch [ "--version" ]
                        (Command.build Version |> Command.expectFlag "version" |> Command.end)
                        Version
            , test "matching non-first element in list" <|
                \() ->
                    expectNoMatch [ "unused", "--version" ] (Command.build Version |> Command.expectFlag "version" |> Command.end)
            , test "command with operand" <|
                \() ->
                    expectMatch [ "http://my-domain.com" ]
                        (Command.build OpenUrl
                            |> Command.with (Option.positionalArg "url")
                            |> Command.end
                        )
                        (OpenUrl "http://my-domain.com")
            , test "command with optional positional arg present" <|
                \() ->
                    expectMatch [ "abcdefg" ]
                        (Command.build identity
                            |> Command.endWith (Option.optionalPositionalArg "revision-range")
                        )
                        (Just "abcdefg")
            , test "command with required and optional positional arg present" <|
                \() ->
                    expectMatch [ "required", "optional" ]
                        (Command.build (,)
                            |> Command.with (Option.positionalArg "required")
                            |> Command.endWith (Option.optionalPositionalArg "revision-range")
                        )
                        ( "required", Just "optional" )
            , test "command with optional positional arg not present" <|
                \() ->
                    expectMatch []
                        (Command.build identity
                            |> Command.endWith (Option.optionalPositionalArg "revision-range")
                        )
                        Nothing
            , test "command with multiple operands" <|
                \() ->
                    expectMatch [ "http://my-domain.com", "./file.txt" ]
                        (Command.build (,)
                            |> Command.with (Option.positionalArg "url")
                            |> Command.with (Option.positionalArg "path/to/file")
                            |> Command.end
                        )
                        ( "http://my-domain.com", "./file.txt" )
            , test "detects that optional flag is absent" <|
                \() ->
                    expectMatch [ "http://my-domain.com" ]
                        (Command.build OpenUrlWithFlag
                            |> Command.with (Option.positionalArg "url")
                            |> Command.with (Option.flag "flag")
                            |> Command.end
                        )
                        (OpenUrlWithFlag "http://my-domain.com" False)
            , test "detects that optional flag is present" <|
                \() ->
                    expectMatch [ "http://my-domain.com", "--flag" ]
                        (Command.build OpenUrlWithFlag
                            |> Command.with (Option.positionalArg "url")
                            |> Command.with (Option.flag "flag")
                            |> Command.end
                        )
                        (OpenUrlWithFlag "http://my-domain.com" True)
            , test "non-matching option" <|
                \() ->
                    expectNoMatch [ "--version" ]
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.end
                        )
            , test "empty args when flag is expected" <|
                \() ->
                    expectNoMatch []
                        (Command.build Help
                            |> Command.expectFlag "help"
                            |> Command.end
                        )
            , test "option with argument" <|
                \() ->
                    expectMatch [ "--name", "Deanna", "--prefix", "Hello" ]
                        (Command.build (,)
                            |> Command.with (Option.requiredKeywordArg "name")
                            |> Command.with (Option.optionalKeywordArg "prefix")
                            |> Command.end
                        )
                        ( "Deanna", Just "Hello" )
            , test "optional option with argument" <|
                \() ->
                    expectMatch [ "--name", "Deanna" ]
                        (Command.build Name
                            |> Command.with (Option.requiredKeywordArg "name")
                            |> Command.end
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
                            |> Command.with (Option.requiredKeywordArg "first-name")
                            |> Command.with (Option.requiredKeywordArg "last-name")
                            |> Command.end
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
                            |> Command.with (Option.requiredKeywordArg "first-name")
                            |> Command.with (Option.requiredKeywordArg "last-name")
                            |> Command.end
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
                            |> Command.with (Option.keywordArgList "header")
                            |> Command.end
                        )
                        [ "abc123", "def456" ]
            , test "doesn't match when unexpected options are present" <|
                \() ->
                    expectNoMatch
                        [ "--verbose"
                        , "--unexpected-option"
                        ]
                        (Command.build identity
                            |> Command.with (Option.flag "verbose")
                            |> Command.end
                        )
            , test "rest operands is empty with no operands" <|
                \() ->
                    expectMatch [ "--verbose" ]
                        (Command.build identity
                            |> Command.expectFlag "verbose"
                            |> Command.endWith (Option.restArgs "files")
                        )
                        []
            , test "rest operands has all operands when there are no required operands" <|
                \() ->
                    expectMatch [ "--verbose", "rest1", "rest2" ]
                        (Command.build identity
                            |> Command.expectFlag "verbose"
                            |> Command.endWith (Option.restArgs "files")
                        )
                        [ "rest1", "rest2" ]
            , test "rest operands has all operands when there is a required operand" <|
                \() ->
                    expectMatch [ "--something", "operand1", "rest1", "rest2" ]
                        (Command.build (,)
                            |> Command.expectFlag "something"
                            |> Command.with (Option.positionalArg "operand")
                            |> Command.endWith (Option.restArgs "files")
                        )
                        ( "operand1", [ "rest1", "rest2" ] )
            ]
        , describe "sub commands"
            [ test "doesn't match if sub command doesn't match" <|
                \() ->
                    expectNoMatch [ "start" ]
                        (Command.buildSubCommand "help" 123
                            |> Command.end
                        )
            , test "matches if sub command is first word" <|
                \() ->
                    expectMatch [ "help" ]
                        (Command.buildSubCommand "help" 123
                            |> Command.end
                        )
                        123
            ]
        , describe "validation"
            [ test "forced err validation makes it not match" <|
                \() ->
                    expectValidationErrors [ "--name", "Bob" ]
                        (Command.build identity
                            |> Command.with
                                (Option.requiredKeywordArg "name"
                                    |> Option.validate (\_ -> Validate.Invalid "Invalid")
                                )
                            |> Command.end
                        )
                        [ { name = "name", invalidReason = "Invalid", valueAsString = toString "Bob" } ]
            , test "validate if present when present and invalid" <|
                \() ->
                    expectValidationErrors [ "--name", "Bob" ]
                        (Command.build identity
                            |> Command.with
                                (Option.optionalKeywordArg "name"
                                    |> Option.validateIfPresent (\_ -> Validate.Invalid "Invalid")
                                )
                            |> Command.end
                        )
                        [ { name = "name", invalidReason = "Invalid", valueAsString = toString (Just "Bob") } ]
            , test "validate if present when absent and invalid" <|
                \() ->
                    expectMatch []
                        (Command.build identity
                            |> Command.with
                                (Option.optionalKeywordArg "name"
                                    |> Option.validateIfPresent (\_ -> Validate.Invalid "Invalid")
                                )
                            |> Command.end
                        )
                        Nothing
            , test "fails when validation function fails" <|
                \() ->
                    expectValidationErrors [ "--name", "Robert" ]
                        (Command.build identity
                            |> Command.with
                                (Option.requiredKeywordArg "name"
                                    |> Option.validate
                                        (\name ->
                                            if String.length name == 3 then
                                                Validate.Valid
                                            else
                                                Validate.Invalid "Must be 3 characters long"
                                        )
                                )
                            |> Command.end
                        )
                        [ { name = "name", invalidReason = "Must be 3 characters long", valueAsString = toString "Robert" } ]
            , test "succeeds when validation function passes" <|
                \() ->
                    expectMatch [ "--name", "Bob" ]
                        (Command.build identity
                            |> Command.with
                                (Option.requiredKeywordArg "name"
                                    |> Option.validate
                                        (\name ->
                                            if String.length name == 3 then
                                                Validate.Valid
                                            else
                                                Validate.Invalid "Must be 3 characters long"
                                        )
                                )
                            |> Command.end
                        )
                        "Bob"
            , test "map validation" <|
                \() ->
                    expectMatch [ "--fuzz", "123" ]
                        (Command.build identity
                            |> Command.with
                                (Option.requiredKeywordArg "fuzz"
                                    |> Option.validateMap String.toInt
                                )
                            |> Command.end
                        )
                        123
            , test "oneOf not default" <|
                \() ->
                    expectMatch [ "--report", "json" ]
                        (Command.build identity
                            |> Command.with
                                (Option.requiredKeywordArg "report"
                                    |> Option.oneOf Console
                                        [ "json" => Json
                                        ]
                                )
                            |> Command.end
                        )
                        Json
            , test "oneOf invalid option" <|
                \() ->
                    expectValidationErrors [ "--report", "invalidOption" ]
                        (Command.build identity
                            |> Command.with
                                (Option.requiredKeywordArg "report"
                                    |> Option.oneOf Console
                                        [ "json" => Json
                                        ]
                                )
                            |> Command.end
                        )
                        [ { name = "report", invalidReason = "Must be one of [json]", valueAsString = "\"invalidOption\"" } ]
            , test "failed map validation" <|
                \() ->
                    expectValidationErrors [ "--fuzz", "abcdefg" ]
                        (Command.build identity
                            |> Command.with
                                (Option.requiredKeywordArg "fuzz"
                                    |> Option.validateMap String.toInt
                                )
                            |> Command.end
                        )
                        [ { name = "fuzz"
                          , invalidReason = "could not convert string 'abcdefg' to an Int"
                          , valueAsString = toString "abcdefg"
                          }
                        ]
            ]
        , describe "mapping"
            [ test "maps operand" <|
                \() ->
                    expectMatch
                        [ "hello" ]
                        (Command.build identity
                            |> Command.with
                                (Option.positionalArg "operand"
                                    |> Option.map String.length
                                )
                            |> Command.end
                        )
                        5
            , test "uses default when option not present" <|
                \() ->
                    expectMatch
                        []
                        (Command.build identity
                            |> Command.with
                                (Option.optionalKeywordArg "output"
                                    |> Option.withDefault "elm.js"
                                )
                            |> Command.end
                        )
                        "elm.js"
            , test "map flag to union" <|
                \() ->
                    expectMatch [ "--watch" ]
                        (Command.build identity
                            |> Command.with
                                (Option.flag "watch"
                                    |> Option.mapFlag
                                        { present = Watch
                                        , absent = NoWatch
                                        }
                                )
                            |> Command.end
                        )
                        Watch
            , test "withDefault uses actual option when option is present" <|
                \() ->
                    expectMatch
                        [ "--output=bundle.js" ]
                        (Command.build identity
                            |> Command.with
                                (Option.optionalKeywordArg "output"
                                    |> Option.withDefault "elm.js"
                                )
                            |> Command.end
                        )
                        "bundle.js"
            , test "hardcoded passes value through" <|
                \() ->
                    expectMatch
                        []
                        (Command.build identity
                            |> Command.hardcoded "hardcoded value"
                            |> Command.end
                        )
                        "hardcoded value"
            ]
        ]


expectMatch : List String -> Command.Command a -> a -> Expectation
expectMatch argv commands expectedValue =
    Command.tryMatch argv commands
        |> Expect.equal (Cli.Command.MatchResult.Match (Ok expectedValue))


expectValidationErrors :
    List String
    -> Command.Command value
    -> List { invalidReason : String, name : String, valueAsString : String }
    -> Expectation
expectValidationErrors argv commands expectedErrors =
    Command.tryMatch argv commands
        |> Expect.equal (Cli.Command.MatchResult.Match (Err expectedErrors))


expectNoMatch : List String -> Command.Command a -> Expectation
expectNoMatch argv commands =
    case Command.tryMatch argv commands of
        Cli.Command.MatchResult.NoMatch unexpectedOptions ->
            Expect.pass

        Cli.Command.MatchResult.Match matchResult ->
            Expect.fail ("Expected no match but got match:\n\n" ++ toString matchResult)
