module OptionsParserTests exposing (all)

import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.MatchResult
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
            [ test "help optionsParser" <|
                \() ->
                    expectMatch [ "--help" ]
                        (OptionsParser.build Help
                            |> OptionsParser.expectFlag "help"
                            |> OptionsParser.end
                        )
                        Help
            , test "version optionsParser" <|
                \() ->
                    expectMatch [ "--version" ]
                        (OptionsParser.build Version |> OptionsParser.expectFlag "version" |> OptionsParser.end)
                        Version
            , test "matching non-first element in list" <|
                \() ->
                    expectNoMatch [ "unused", "--version" ] (OptionsParser.build Version |> OptionsParser.expectFlag "version" |> OptionsParser.end)
            , test "optionsParser with operand" <|
                \() ->
                    expectMatch [ "http://my-domain.com" ]
                        (OptionsParser.build OpenUrl
                            |> OptionsParser.with (Option.positionalArg "url")
                            |> OptionsParser.end
                        )
                        (OpenUrl "http://my-domain.com")
            , test "optionsParser with optional positional arg present" <|
                \() ->
                    expectMatch [ "abcdefg" ]
                        (OptionsParser.build identity
                            |> OptionsParser.optionalPositionalArg (Option.optionalPositionalArg "revision-range")
                        )
                        (Just "abcdefg")
            , test "optionsParser with required and optional positional arg present" <|
                \() ->
                    expectMatch [ "required", "optional" ]
                        (OptionsParser.build (,)
                            |> OptionsParser.with (Option.positionalArg "required")
                            |> OptionsParser.optionalPositionalArg (Option.optionalPositionalArg "revision-range")
                        )
                        ( "required", Just "optional" )
            , test "optionsParser with optional positional arg not present" <|
                \() ->
                    expectMatch []
                        (OptionsParser.build identity
                            |> OptionsParser.optionalPositionalArg (Option.optionalPositionalArg "revision-range")
                        )
                        Nothing
            , test "optionsParser with multiple operands" <|
                \() ->
                    expectMatch [ "http://my-domain.com", "./file.txt" ]
                        (OptionsParser.build (,)
                            |> OptionsParser.with (Option.positionalArg "url")
                            |> OptionsParser.with (Option.positionalArg "path/to/file")
                            |> OptionsParser.end
                        )
                        ( "http://my-domain.com", "./file.txt" )
            , test "detects that optional flag is absent" <|
                \() ->
                    expectMatch [ "http://my-domain.com" ]
                        (OptionsParser.build OpenUrlWithFlag
                            |> OptionsParser.with (Option.positionalArg "url")
                            |> OptionsParser.with (Option.flag "flag")
                            |> OptionsParser.end
                        )
                        (OpenUrlWithFlag "http://my-domain.com" False)
            , test "detects that optional flag is present" <|
                \() ->
                    expectMatch [ "http://my-domain.com", "--flag" ]
                        (OptionsParser.build OpenUrlWithFlag
                            |> OptionsParser.with (Option.positionalArg "url")
                            |> OptionsParser.with (Option.flag "flag")
                            |> OptionsParser.end
                        )
                        (OpenUrlWithFlag "http://my-domain.com" True)
            , test "non-matching option" <|
                \() ->
                    expectNoMatch [ "--version" ]
                        (OptionsParser.build Help
                            |> OptionsParser.expectFlag "help"
                            |> OptionsParser.end
                        )
            , test "empty args when flag is expected" <|
                \() ->
                    expectNoMatch []
                        (OptionsParser.build Help
                            |> OptionsParser.expectFlag "help"
                            |> OptionsParser.end
                        )
            , test "option with argument" <|
                \() ->
                    expectMatch [ "--name", "Deanna", "--prefix", "Hello" ]
                        (OptionsParser.build (,)
                            |> OptionsParser.with (Option.requiredKeywordArg "name")
                            |> OptionsParser.with (Option.optionalKeywordArg "prefix")
                            |> OptionsParser.end
                        )
                        ( "Deanna", Just "Hello" )
            , test "optional option with argument" <|
                \() ->
                    expectMatch [ "--name", "Deanna" ]
                        (OptionsParser.build Name
                            |> OptionsParser.with (Option.requiredKeywordArg "name")
                            |> OptionsParser.end
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
                        (OptionsParser.build FullName
                            |> OptionsParser.with (Option.requiredKeywordArg "first-name")
                            |> OptionsParser.with (Option.requiredKeywordArg "last-name")
                            |> OptionsParser.end
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
                        (OptionsParser.build FullName
                            |> OptionsParser.with (Option.requiredKeywordArg "first-name")
                            |> OptionsParser.with (Option.requiredKeywordArg "last-name")
                            |> OptionsParser.end
                        )
            , test "extracts multiple params" <|
                \() ->
                    expectMatch
                        [ "--header"
                        , "abc123"
                        , "--header"
                        , "def456"
                        ]
                        (OptionsParser.build identity
                            |> OptionsParser.with (Option.keywordArgList "header")
                            |> OptionsParser.end
                        )
                        [ "abc123", "def456" ]
            , test "doesn't match when unexpected options are present" <|
                \() ->
                    expectNoMatch
                        [ "--verbose"
                        , "--unexpected-option"
                        ]
                        (OptionsParser.build identity
                            |> OptionsParser.with (Option.flag "verbose")
                            |> OptionsParser.end
                        )
            , test "rest operands is empty with no operands" <|
                \() ->
                    expectMatch [ "--verbose" ]
                        (OptionsParser.build identity
                            |> OptionsParser.expectFlag "verbose"
                            |> OptionsParser.restArgs (Option.restArgs "files")
                        )
                        []
            , test "rest operands has all operands when there are no required operands" <|
                \() ->
                    expectMatch [ "--verbose", "rest1", "rest2" ]
                        (OptionsParser.build identity
                            |> OptionsParser.expectFlag "verbose"
                            |> OptionsParser.restArgs (Option.restArgs "files")
                        )
                        [ "rest1", "rest2" ]
            , test "rest operands has all operands when there is a required operand" <|
                \() ->
                    expectMatch [ "--something", "operand1", "rest1", "rest2" ]
                        (OptionsParser.build (,)
                            |> OptionsParser.expectFlag "something"
                            |> OptionsParser.with (Option.positionalArg "operand")
                            |> OptionsParser.restArgs (Option.restArgs "files")
                        )
                        ( "operand1", [ "rest1", "rest2" ] )
            ]
        , describe "sub optionsParsers"
            [ test "doesn't match if sub optionsParser doesn't match" <|
                \() ->
                    expectNoMatch [ "start" ]
                        (OptionsParser.buildSubCommand "help" 123
                            |> OptionsParser.end
                        )
            , test "matches if sub optionsParser is first word" <|
                \() ->
                    expectMatch [ "help" ]
                        (OptionsParser.buildSubCommand "help" 123
                            |> OptionsParser.end
                        )
                        123
            ]
        , describe "validation"
            [ test "forced err validation makes it not match" <|
                \() ->
                    expectValidationErrors [ "--name", "Bob" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "name"
                                    |> Option.validate (\_ -> Validate.Invalid "Invalid")
                                )
                            |> OptionsParser.end
                        )
                        [ { name = "name", invalidReason = "Invalid", valueAsString = toString "Bob" } ]
            , test "validate if present when present and invalid" <|
                \() ->
                    expectValidationErrors [ "--name", "Bob" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.optionalKeywordArg "name"
                                    |> Option.validateIfPresent (\_ -> Validate.Invalid "Invalid")
                                )
                            |> OptionsParser.end
                        )
                        [ { name = "name", invalidReason = "Invalid", valueAsString = toString (Just "Bob") } ]
            , test "validate if present when absent and invalid" <|
                \() ->
                    expectMatch []
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.optionalKeywordArg "name"
                                    |> Option.validateIfPresent (\_ -> Validate.Invalid "Invalid")
                                )
                            |> OptionsParser.end
                        )
                        Nothing
            , test "fails when validation function fails" <|
                \() ->
                    expectValidationErrors [ "--name", "Robert" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "name"
                                    |> Option.validate
                                        (\name ->
                                            if String.length name == 3 then
                                                Validate.Valid
                                            else
                                                Validate.Invalid "Must be 3 characters long"
                                        )
                                )
                            |> OptionsParser.end
                        )
                        [ { name = "name", invalidReason = "Must be 3 characters long", valueAsString = toString "Robert" } ]
            , test "succeeds when validation function passes" <|
                \() ->
                    expectMatch [ "--name", "Bob" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "name"
                                    |> Option.validate
                                        (\name ->
                                            if String.length name == 3 then
                                                Validate.Valid
                                            else
                                                Validate.Invalid "Must be 3 characters long"
                                        )
                                )
                            |> OptionsParser.end
                        )
                        "Bob"
            , test "map validation" <|
                \() ->
                    expectMatch [ "--fuzz", "123" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "fuzz"
                                    |> Option.validateMap String.toInt
                                )
                            |> OptionsParser.end
                        )
                        123
            , test "oneOf not default" <|
                \() ->
                    expectMatch [ "--report", "json" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "report"
                                    |> Option.oneOf Console
                                        [ "json" => Json
                                        ]
                                )
                            |> OptionsParser.end
                        )
                        Json
            , test "oneOf invalid option" <|
                \() ->
                    expectValidationErrors [ "--report", "invalidOption" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "report"
                                    |> Option.oneOf Console
                                        [ "json" => Json
                                        ]
                                )
                            |> OptionsParser.end
                        )
                        [ { name = "report", invalidReason = "Must be one of [json]", valueAsString = "\"invalidOption\"" } ]
            , test "failed map validation" <|
                \() ->
                    expectValidationErrors [ "--fuzz", "abcdefg" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "fuzz"
                                    |> Option.validateMap String.toInt
                                )
                            |> OptionsParser.end
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
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.positionalArg "operand"
                                    |> Option.map String.length
                                )
                            |> OptionsParser.end
                        )
                        5
            , test "uses default when option not present" <|
                \() ->
                    expectMatch
                        []
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.optionalKeywordArg "output"
                                    |> Option.withDefault "elm.js"
                                )
                            |> OptionsParser.end
                        )
                        "elm.js"
            , test "map flag to union" <|
                \() ->
                    expectMatch [ "--watch" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.flag "watch"
                                    |> Option.mapFlag
                                        { present = Watch
                                        , absent = NoWatch
                                        }
                                )
                            |> OptionsParser.end
                        )
                        Watch
            , test "withDefault uses actual option when option is present" <|
                \() ->
                    expectMatch
                        [ "--output=bundle.js" ]
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.optionalKeywordArg "output"
                                    |> Option.withDefault "elm.js"
                                )
                            |> OptionsParser.end
                        )
                        "bundle.js"
            , test "hardcoded passes value through" <|
                \() ->
                    expectMatch
                        []
                        (OptionsParser.build identity
                            |> OptionsParser.hardcoded "hardcoded value"
                            |> OptionsParser.end
                        )
                        "hardcoded value"
            ]
        ]


expectMatch : List String -> OptionsParser.OptionsParser a builderState -> a -> Expectation
expectMatch argv optionsParsers expectedValue =
    OptionsParser.tryMatch argv optionsParsers
        |> Expect.equal (Cli.OptionsParser.MatchResult.Match (Ok expectedValue))


expectValidationErrors :
    List String
    -> OptionsParser.OptionsParser value builderState
    -> List { invalidReason : String, name : String, valueAsString : String }
    -> Expectation
expectValidationErrors argv optionsParsers expectedErrors =
    OptionsParser.tryMatch argv optionsParsers
        |> Expect.equal (Cli.OptionsParser.MatchResult.Match (Err expectedErrors))


expectNoMatch : List String -> OptionsParser.OptionsParser a builderState -> Expectation
expectNoMatch argv optionsParsers =
    case OptionsParser.tryMatch argv optionsParsers of
        Cli.OptionsParser.MatchResult.NoMatch unexpectedOptions ->
            Expect.pass

        Cli.OptionsParser.MatchResult.Match matchResult ->
            Expect.fail ("Expected no match but got match:\n\n" ++ toString matchResult)
