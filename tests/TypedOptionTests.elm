module TypedOptionTests exposing (all)

import Cli.Option as UntypedOption
import Cli.Option.Typed as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Expect
import Json.Encode as Encode
import Test exposing (..)
import TsJson.Decode as TsDecode


all : Test
all =
    describe "Cli.Option.Typed"
        [ test "requiredKeywordArg with string decoder parses CLI arg" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "name" TsDecode.string)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "--name", "hello" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch "hello")
        , test "requiredKeywordArg with int decoder parses CLI arg" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "count" TsDecode.int)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "--count", "42" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch 42)
        , test "requiredKeywordArg with int decoder works in JSON mode" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "count" TsDecode.int)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"count\":42}" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch 42)
        , test "requiredKeywordArg produces correct JSON schema" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "count" TsDecode.int)
                        )
                    |> Program.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal
                        (Encode.object
                            [ ( "$cli", Encode.string "elm-cli-options-parser" )
                            , ( "type", Encode.string "object" )
                            , ( "properties"
                              , Encode.object
                                    [ ( "count", Encode.object [ ( "type", Encode.string "integer" ) ] ) ]
                              )
                            , ( "required", Encode.list Encode.string [ "count" ] )
                            ]
                            |> Encode.encode 0
                        )
        , test "requiredKeywordArg int gives clear CLI error for non-numeric" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "count" TsDecode.int)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "--count", "abc" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> expectFailureContaining "Expecting an INT"
        , test "optionalKeywordArg with string returns Just when present" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.optionalKeywordArg "greeting" TsDecode.string)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "--greeting", "hi" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch (Just "hi"))
        , test "optionalKeywordArg returns Nothing when absent" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.optionalKeywordArg "greeting" TsDecode.string)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch Nothing)
        , test "re-exported oneOf works without separate import" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "format" TsDecode.string
                                    |> Option.oneOf
                                        [ ( "json", "JSON" )
                                        , ( "csv", "CSV" )
                                        ]
                                )
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "--format", "json" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch "JSON")
        , test "flag works" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with (Option.flag "verbose")
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "--verbose" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch True)
        , test "re-exported withDescription works without separate import" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredKeywordArg "count" TsDecode.int
                                    |> Option.withDescription "Number of items"
                                )
                        )
                    |> Program.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal
                        (Encode.object
                            [ ( "$cli", Encode.string "elm-cli-options-parser" )
                            , ( "type", Encode.string "object" )
                            , ( "properties"
                              , Encode.object
                                    [ ( "count"
                                      , Encode.object
                                            [ ( "type", Encode.string "integer" )
                                            , ( "description", Encode.string "Number of items" )
                                            ]
                                      )
                                    ]
                              )
                            , ( "required", Encode.list Encode.string [ "count" ] )
                            ]
                            |> Encode.encode 0
                        )
        , test "requiredPositionalArg with string decoder" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredPositionalArg "file" TsDecode.string)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "hello.txt" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch "hello.txt")
        , test "requiredPositionalArg with int decoder" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.requiredPositionalArg "port" TsDecode.int)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "8080" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch 8080)
        , test "optionalPositionalArg returns Just when present" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.withOptionalPositionalArg
                                (Option.optionalPositionalArg "revision" TsDecode.string)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "abc123" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch (Just "abc123"))
        , test "optionalPositionalArg returns Nothing when absent" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.withOptionalPositionalArg
                                (Option.optionalPositionalArg "revision" TsDecode.string)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch Nothing)
        , test "restArgs collects remaining positional args" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.withRestArgs
                                (Option.restArgs "files")
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "a.txt", "b.txt" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch [ "a.txt", "b.txt" ])
        , test "keywordArgList collects repeated keyword args" <|
            \() ->
                Program.config
                    |> Program.add
                        (OptionsParser.build identity
                            |> OptionsParser.with
                                (Option.keywordArgList "header" TsDecode.string)
                        )
                    |> (\cfg ->
                            Program.run cfg
                                [ "node", "test", "--header", "X-A: 1", "--header", "X-B: 2" ]
                                "1.0.0"
                                Program.WithoutColor
                       )
                    |> Expect.equal (Program.CustomMatch [ "X-A: 1", "X-B: 2" ])
        ]


expectFailureContaining : String -> Program.RunResult msg -> Expect.Expectation
expectFailureContaining substring result =
    case result of
        Program.SystemMessage Program.Failure message ->
            if String.contains substring message then
                Expect.pass

            else
                Expect.fail ("Expected error containing \"" ++ substring ++ "\" but got:\n" ++ message)

        other ->
            Expect.fail ("Expected SystemMessage Failure but got: " ++ Debug.toString other)
