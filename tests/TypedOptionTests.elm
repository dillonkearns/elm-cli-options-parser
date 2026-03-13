module TypedOptionTests exposing (all)

import Cli.Option
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
        [ describe "Option.string"
            [ test "parses bare CLI arg" <|
                \() ->
                    runWith (Option.requiredKeywordArg "name" Option.string)
                        [ "--name", "hello" ]
                        |> Expect.equal (Program.CustomMatch "hello")
            , test "preserves numeric-looking input as string" <|
                \() ->
                    runWith (Option.requiredKeywordArg "name" Option.string)
                        [ "--name", "42" ]
                        |> Expect.equal (Program.CustomMatch "42")
            , test "preserves 'true' as string" <|
                \() ->
                    runWith (Option.requiredKeywordArg "name" Option.string)
                        [ "--name", "true" ]
                        |> Expect.equal (Program.CustomMatch "true")
            , test "preserves 'null' as string" <|
                \() ->
                    runWith (Option.requiredKeywordArg "name" Option.string)
                        [ "--name", "null" ]
                        |> Expect.equal (Program.CustomMatch "null")
            , test "preserves quotes in input" <|
                \() ->
                    runWith (Option.requiredKeywordArg "name" Option.string)
                        [ "--name", "\"quoted\"" ]
                        |> Expect.equal (Program.CustomMatch "\"quoted\"")
            , test "preserves spaces and special chars" <|
                \() ->
                    runWith (Option.requiredKeywordArg "msg" Option.string)
                        [ "--msg", "hello world!" ]
                        |> Expect.equal (Program.CustomMatch "hello world!")
            , test "works in JSON mode" <|
                \() ->
                    runJsonWith (Option.requiredKeywordArg "name" Option.string)
                        [ ( "name", Encode.string "hello" ) ]
                        |> Expect.equal (Program.CustomMatch "hello")
            , test "produces string schema" <|
                \() ->
                    schemaFor (Option.requiredKeywordArg "name" Option.string)
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string "test --name <NAME>" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "keywordValues"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Keyword arguments with values (e.g., --name <value>)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "name", Encode.object [ ( "type", Encode.string "string" ) ] ) ]
                                                                  )
                                                                , ( "required", Encode.list Encode.string [ "name" ] )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "keywordValues" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
            ]
        , describe "Option.int"
            [ test "parses numeric CLI arg" <|
                \() ->
                    runWith (Option.requiredKeywordArg "count" Option.int)
                        [ "--count", "42" ]
                        |> Expect.equal (Program.CustomMatch 42)
            , test "parses negative int" <|
                \() ->
                    runWith (Option.requiredKeywordArg "count" Option.int)
                        [ "--count", "-7" ]
                        |> Expect.equal (Program.CustomMatch -7)
            , test "rejects float" <|
                \() ->
                    runWith (Option.requiredKeywordArg "count" Option.int)
                        [ "--count", "3.14" ]
                        |> expectFailure
            , test "rejects bare text with clear error" <|
                \() ->
                    runWith (Option.requiredKeywordArg "count" Option.int)
                        [ "--count", "abc" ]
                        |> expectFailure
            , test "rejects 'true'" <|
                \() ->
                    runWith (Option.requiredKeywordArg "count" Option.int)
                        [ "--count", "true" ]
                        |> expectFailure
            , test "works in JSON mode" <|
                \() ->
                    runJsonWith (Option.requiredKeywordArg "count" Option.int)
                        [ ( "count", Encode.int 42 ) ]
                        |> Expect.equal (Program.CustomMatch 42)
            , test "JSON mode rejects string" <|
                \() ->
                    runJsonWith (Option.requiredKeywordArg "count" Option.int)
                        [ ( "count", Encode.string "abc" ) ]
                        |> expectFailure
            , test "produces integer schema" <|
                \() ->
                    schemaFor (Option.requiredKeywordArg "count" Option.int)
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string "test --count <COUNT>" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "keywordValues"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Keyword arguments with values (e.g., --name <value>)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "count", Encode.object [ ( "type", Encode.string "integer" ) ] ) ]
                                                                  )
                                                                , ( "required", Encode.list Encode.string [ "count" ] )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "keywordValues" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
            ]
        , describe "Option.float"
            [ test "parses float CLI arg" <|
                \() ->
                    runWith (Option.requiredKeywordArg "rate" Option.float)
                        [ "--rate", "3.14" ]
                        |> Expect.equal (Program.CustomMatch 3.14)
            , test "parses integer as float" <|
                \() ->
                    runWith (Option.requiredKeywordArg "rate" Option.float)
                        [ "--rate", "42" ]
                        |> Expect.equal (Program.CustomMatch 42.0)
            , test "rejects bare text" <|
                \() ->
                    runWith (Option.requiredKeywordArg "rate" Option.float)
                        [ "--rate", "abc" ]
                        |> expectFailure
            ]
        , describe "Option.bool"
            [ test "parses 'true'" <|
                \() ->
                    runWith (Option.requiredKeywordArg "dry" Option.bool)
                        [ "--dry", "true" ]
                        |> Expect.equal (Program.CustomMatch True)
            , test "parses 'false'" <|
                \() ->
                    runWith (Option.requiredKeywordArg "dry" Option.bool)
                        [ "--dry", "false" ]
                        |> Expect.equal (Program.CustomMatch False)
            , test "rejects bare text" <|
                \() ->
                    runWith (Option.requiredKeywordArg "dry" Option.bool)
                        [ "--dry", "abc" ]
                        |> expectFailure
            ]
        , describe "fromDecoder"
            [ test "custom decoder works in JSON mode" <|
                \() ->
                    let
                        pointDecoder =
                            TsDecode.succeed (\x y -> ( x, y ))
                                |> TsDecode.andMap (TsDecode.field "x" TsDecode.int)
                                |> TsDecode.andMap (TsDecode.field "y" TsDecode.int)
                    in
                    runJsonWith (Option.requiredKeywordArg "point" (Option.fromDecoder pointDecoder))
                        [ ( "point", Encode.object [ ( "x", Encode.int 1 ), ( "y", Encode.int 2 ) ] ) ]
                        |> Expect.equal (Program.CustomMatch ( 1, 2 ))
            , test "custom decoder in CLI mode expects strict JSON" <|
                \() ->
                    let
                        pointDecoder =
                            TsDecode.succeed (\x y -> ( x, y ))
                                |> TsDecode.andMap (TsDecode.field "x" TsDecode.int)
                                |> TsDecode.andMap (TsDecode.field "y" TsDecode.int)
                    in
                    runWith (Option.requiredKeywordArg "point" (Option.fromDecoder pointDecoder))
                        [ "--point", "{\"x\":1,\"y\":2}" ]
                        |> Expect.equal (Program.CustomMatch ( 1, 2 ))
            , test "fromDecoder TsDecode.string in CLI mode requires JSON-quoted string" <|
                \() ->
                    -- bare text is NOT valid JSON — this should fail
                    runWith (Option.requiredKeywordArg "name" (Option.fromDecoder TsDecode.string))
                        [ "--name", "hello" ]
                        |> expectFailure
            , test "fromDecoder TsDecode.string in CLI mode accepts JSON-quoted string" <|
                \() ->
                    -- JSON string: "hello" (with quotes on CLI)
                    runWith (Option.requiredKeywordArg "name" (Option.fromDecoder TsDecode.string))
                        [ "--name", "\"hello\"" ]
                        |> Expect.equal (Program.CustomMatch "hello")
            ]
        , describe "optionalKeywordArg"
            [ test "returns Just when present" <|
                \() ->
                    runWith (Option.optionalKeywordArg "greeting" Option.string)
                        [ "--greeting", "hi" ]
                        |> Expect.equal (Program.CustomMatch (Just "hi"))
            , test "returns Nothing when absent" <|
                \() ->
                    runWith (Option.optionalKeywordArg "greeting" Option.string)
                        [{- absent -}]
                        |> Expect.equal (Program.CustomMatch Nothing)
            , test "optional int present" <|
                \() ->
                    runWith (Option.optionalKeywordArg "count" Option.int)
                        [ "--count", "42" ]
                        |> Expect.equal (Program.CustomMatch (Just 42))
            , test "optional int invalid gives error" <|
                \() ->
                    runWith (Option.optionalKeywordArg "count" Option.int)
                        [ "--count", "abc" ]
                        |> expectFailure
            ]
        , describe "keywordArgList"
            [ test "collects repeated args" <|
                \() ->
                    runWith (Option.keywordArgList "header" Option.string)
                        [ "--header", "X-A: 1", "--header", "X-B: 2" ]
                        |> Expect.equal (Program.CustomMatch [ "X-A: 1", "X-B: 2" ])
            ]
        , describe "requiredPositionalArg"
            [ test "string positional" <|
                \() ->
                    runWith (Option.requiredPositionalArg "file" Option.string)
                        [ "hello.txt" ]
                        |> Expect.equal (Program.CustomMatch "hello.txt")
            , test "int positional" <|
                \() ->
                    runWith (Option.requiredPositionalArg "port" Option.int)
                        [ "8080" ]
                        |> Expect.equal (Program.CustomMatch 8080)
            ]
        , describe "optionalPositionalArg"
            [ test "returns Just when present" <|
                \() ->
                    runOptionalPositionalWith (Option.optionalPositionalArg "revision" Option.string)
                        [ "abc123" ]
                        |> Expect.equal (Program.CustomMatch (Just "abc123"))
            , test "returns Nothing when absent" <|
                \() ->
                    runOptionalPositionalWith (Option.optionalPositionalArg "revision" Option.string)
                        []
                        |> Expect.equal (Program.CustomMatch Nothing)
            ]
        , describe "flag and restArgs (no decoder)"
            [ test "flag works" <|
                \() ->
                    runWith (Option.flag "verbose")
                        [ "--verbose" ]
                        |> Expect.equal (Program.CustomMatch True)
            , test "restArgs collects remaining args" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.withRestArgs (Option.restArgs "files")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "a.txt", "b.txt" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch [ "a.txt", "b.txt" ])
            ]
        , describe "JSON input with $cli object"
            [ test "keyword list via $cli.keywordLists" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli"
                                  , Encode.object
                                        [ ( "keywordLists"
                                          , Encode.object
                                                [ ( "header", Encode.list Encode.string [ "X-A: 1", "X-B: 2" ] ) ]
                                          )
                                        ]
                                  )
                                ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.keywordArgList "header" Option.string)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch [ "X-A: 1", "X-B: 2" ])
            , test "keyword list absent in $cli.keywordLists defaults to empty" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli", Encode.object [] ) ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.keywordArgList "header" Option.string)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch [])
            , test "positional arg via $cli.positional" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli"
                                  , Encode.object
                                        [ ( "positional", Encode.list Encode.string [ "hello.txt" ] ) ]
                                  )
                                ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredPositionalArg "file" Option.string)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch "hello.txt")
            , test "multiple positional args via $cli.positional" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli"
                                  , Encode.object
                                        [ ( "positional", Encode.list Encode.string [ "src.txt", "dest.txt" ] ) ]
                                  )
                                ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build Tuple.pair
                                |> OptionsParser.with (Option.requiredPositionalArg "source" Option.string)
                                |> OptionsParser.with (Option.requiredPositionalArg "dest" Option.string)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch ( "src.txt", "dest.txt" ))
            , test "rest args via $cli.positional tail" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli"
                                  , Encode.object
                                        [ ( "positional", Encode.list Encode.string [ "a.txt", "b.txt", "c.txt" ] ) ]
                                  )
                                ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build Tuple.pair
                                |> OptionsParser.with (Option.requiredPositionalArg "source" Option.string)
                                |> OptionsParser.withRestArgs (Option.restArgs "files")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch ( "a.txt", [ "b.txt", "c.txt" ] ))
            , test "rest args only (no fixed positional) via $cli.positional" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli"
                                  , Encode.object
                                        [ ( "positional", Encode.list Encode.string [ "x.txt", "y.txt" ] ) ]
                                  )
                                ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.withRestArgs (Option.restArgs "files")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch [ "x.txt", "y.txt" ])
            , test "flag via $cli.flags" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli"
                                  , Encode.object
                                        [ ( "flags", Encode.object [ ( "verbose", Encode.bool True ) ] ) ]
                                  )
                                ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.flag "verbose")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch True)
            , test "flag absent from $cli.flags defaults to False" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli", Encode.object [] ) ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.flag "verbose")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal (Program.CustomMatch False)
            , test "mixed: positional + keyword + flag + keyword list" <|
                \() ->
                    let
                        jsonArg =
                            Encode.object
                                [ ( "$cli"
                                  , Encode.object
                                        [ ( "positional", Encode.list Encode.string [ "input.txt" ] )
                                        , ( "flags", Encode.object [ ( "verbose", Encode.bool True ) ] )
                                        , ( "keywordLists"
                                          , Encode.object
                                                [ ( "header", Encode.list Encode.string [ "X-A: 1" ] ) ]
                                          )
                                        , ( "keywordValues"
                                          , Encode.object
                                                [ ( "limit", Encode.string "10" ) ]
                                          )
                                        ]
                                  )
                                ]
                                |> Encode.encode 0
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build (\file limit verbose headers -> ( ( file, limit ), ( verbose, headers ) ))
                                |> OptionsParser.with (Option.requiredPositionalArg "file" Option.string)
                                |> OptionsParser.with (Option.requiredKeywordArg "limit" Option.string)
                                |> OptionsParser.with (Option.flag "verbose")
                                |> OptionsParser.with (Option.keywordArgList "header" Option.string)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", jsonArg ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch
                                ( ( "input.txt", "10" ), ( True, [ "X-A: 1" ] ) )
                            )
            ]
        , describe "modifiers"
            [ test "oneOf works" <|
                \() ->
                    runWith
                        (Option.requiredKeywordArg "format" Option.string
                            |> Option.oneOf
                                [ ( "json", "JSON" )
                                , ( "csv", "CSV" )
                                ]
                        )
                        [ "--format", "json" ]
                        |> Expect.equal (Program.CustomMatch "JSON")
            , test "withDescription adds to schema" <|
                \() ->
                    schemaFor
                        (Option.requiredKeywordArg "count" Option.int
                            |> Option.withDescription "Number of items"
                        )
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string "test --count <COUNT>" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "keywordValues"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Keyword arguments with values (e.g., --name <value>)" )
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
                                                          )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "keywordValues" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
            ]
        ]



-- Test helpers


runWith : Option.Option from to { c | position : Cli.Option.BeginningOption } -> List String -> Program.RunResult to
runWith option args =
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.with option
            )
        |> (\cfg ->
                Program.run cfg
                    ([ "node", "test" ] ++ args)
                    "1.0.0"
                    Program.WithoutColor
           )


runOptionalPositionalWith : Option.Option from to { c | position : Cli.Option.OptionalPositionalArgOption } -> List String -> Program.RunResult to
runOptionalPositionalWith option args =
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.withOptionalPositionalArg option
            )
        |> (\cfg ->
                Program.run cfg
                    ([ "node", "test" ] ++ args)
                    "1.0.0"
                    Program.WithoutColor
           )


runJsonWith : Option.Option from to { c | position : Cli.Option.BeginningOption } -> List ( String, Encode.Value ) -> Program.RunResult to
runJsonWith option fields =
    let
        jsonArg =
            Encode.object
                [ ( "$cli"
                  , Encode.object
                        [ ( "keywordValues", Encode.object fields ) ]
                  )
                ]
                |> Encode.encode 0
    in
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.with option
            )
        |> (\cfg ->
                Program.run cfg
                    [ "node", "test", jsonArg ]
                    "1.0.0"
                    Program.WithoutColor
           )


schemaFor : Option.Option from to { c | position : Cli.Option.BeginningOption } -> String
schemaFor option =
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.with option
            )
        |> Program.toJsonSchema "test"
        |> Encode.encode 0


expectFailure : Program.RunResult msg -> Expect.Expectation
expectFailure result =
    case result of
        Program.SystemMessage Program.Failure _ ->
            Expect.pass

        other ->
            Expect.fail ("Expected SystemMessage Failure but got: " ++ Debug.toString other)


expectFailureContaining : String -> Program.RunResult msg -> Expect.Expectation
expectFailureContaining substring result =
    case result of
        Program.SystemMessage Program.Failure message ->
            if String.contains substring (String.toUpper message) then
                Expect.pass

            else
                Expect.fail ("Expected error containing \"" ++ substring ++ "\" but got:\n" ++ message)

        other ->
            Expect.fail ("Expected SystemMessage Failure but got: " ++ Debug.toString other)
