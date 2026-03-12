module JsonSchemaTests exposing (all)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Expect
import Json.Encode as Encode
import Test exposing (..)
import TsJson.Decode as TsDecode


all : Test
all =
    describe "toJsonSchema"
        [ describe "single parser"
            [ test "single required keyword arg" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "name", [ ( "type", Encode.string "string" ) ] ) ]
                            , required = [ "name" ]
                            }
            , test "optional keyword arg is not required" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "greeting", [ ( "type", Encode.string "string" ) ] ) ]
                            , required = []
                            }
            , test "flag is boolean and not required" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.flag "verbose")
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "verbose", [ ( "type", Encode.string "boolean" ) ] ) ]
                            , required = []
                            }
            , test "required positional arg" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredPositionalArg "file")
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "file", [ ( "type", Encode.string "string" ) ] ) ]
                            , required = [ "file" ]
                            }
            , test "optional positional arg" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.withOptionalPositionalArg (Option.optionalPositionalArg "revision")
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "revision", [ ( "type", Encode.string "string" ) ] ) ]
                            , required = []
                            }
            , test "rest args is array of strings" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.withRestArgs (Option.restArgs "files")
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "files"
                                  , [ ( "type", Encode.string "array" )
                                    , ( "items", Encode.object [ ( "type", Encode.string "string" ) ] )
                                    ]
                                  )
                                ]
                            , required = []
                            }
            , test "keyword arg list is array of strings" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.keywordArgList "header")
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "header"
                                  , [ ( "type", Encode.string "array" )
                                    , ( "items", Encode.object [ ( "type", Encode.string "string" ) ] )
                                    ]
                                  )
                                ]
                            , required = []
                            }
            , test "description is included" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with
                                    (Option.requiredKeywordArg "name"
                                        |> Option.withDescription "The user's name"
                                    )
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "name"
                                  , [ ( "type", Encode.string "string" )
                                    , ( "description", Encode.string "The user's name" )
                                    ]
                                  )
                                ]
                            , required = [ "name" ]
                            }
            , test "oneOf adds anyOf with const values" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with
                                    (Option.requiredKeywordArg "format"
                                        |> Option.oneOf
                                            [ ( "json", () )
                                            , ( "junit", () )
                                            , ( "console", () )
                                            ]
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
                                        [ ( "format"
                                          , Encode.object
                                                [ ( "anyOf"
                                                  , Encode.list identity
                                                        [ Encode.object [ ( "const", Encode.string "json" ) ]
                                                        , Encode.object [ ( "const", Encode.string "junit" ) ]
                                                        , Encode.object [ ( "const", Encode.string "console" ) ]
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "format" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "mixed options - required and optional together" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build (\a b c -> ( a, b, c ))
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                                |> OptionsParser.with (Option.flag "verbose")
                            )
                        |> expectJsonSchema
                            { properties =
                                [ ( "name", [ ( "type", Encode.string "string" ) ] )
                                , ( "greeting", [ ( "type", Encode.string "string" ) ] )
                                , ( "verbose", [ ( "type", Encode.string "boolean" ) ] )
                                ]
                            , required = [ "name" ]
                            }
            , test "no options produces empty object schema" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build ())
                        |> expectJsonSchema
                            { properties = []
                            , required = []
                            }
            ]
        , describe "subcommands"
            [ test "single subcommand includes subcommand property" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.buildSubCommand "init" identity
                                |> OptionsParser.with (Option.flag "bare")
                            )
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$cli", Encode.string "elm-cli-options-parser" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "init" ) ] )
                                        , ( "bare", Encode.object [ ( "type", Encode.string "boolean" ) ] )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "subcommand" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "multiple subcommands produce anyOf" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.buildSubCommand "init" ()
                                |> OptionsParser.map (\_ -> ())
                            )
                        |> Program.add
                            (OptionsParser.buildSubCommand "clone" identity
                                |> OptionsParser.with (Option.requiredPositionalArg "repository")
                                |> OptionsParser.map (\_ -> ())
                            )
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$cli", Encode.string "elm-cli-options-parser" )
                                , ( "anyOf"
                                  , Encode.list identity
                                        [ Encode.object
                                            [ ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "init" ) ] ) ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "subcommand" ] )
                                            ]
                                        , Encode.object
                                            [ ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "clone" ) ] )
                                                    , ( "repository", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "subcommand", "repository" ] )
                                            ]
                                        ]
                                  )
                                ]
                                |> Encode.encode 0
                            )
            ]
        , describe "withTypedJson"
            [ test "withTypedJson embeds JSON schema for keyword arg" <|
                \() ->
                    let
                        todoDecoder =
                            TsDecode.map2 (\title desc -> { title = title, description = desc })
                                (TsDecode.field "title" TsDecode.string)
                                (TsDecode.field "description" TsDecode.string)
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with
                                    (Option.requiredKeywordArg "todo"
                                        |> Option.withTypedJson todoDecoder
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
                                        [ ( "todo"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "description", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                        , ( "title", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "description", "title" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "todo" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "withTypedJson description is merged into embedded schema" <|
                \() ->
                    let
                        todoDecoder =
                            TsDecode.map2 (\title desc -> { title = title, description = desc })
                                (TsDecode.field "title" TsDecode.string)
                                (TsDecode.field "description" TsDecode.string)
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with
                                    (Option.requiredKeywordArg "todo"
                                        |> Option.withTypedJson todoDecoder
                                        |> Option.withDescription "The todo item"
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
                                        [ ( "todo"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "description", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                        , ( "title", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "description", "title" ] )
                                                , ( "description", Encode.string "The todo item" )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "todo" ] )
                                ]
                                |> Encode.encode 0
                            )
            ]
        , describe "typed constructors"
            [ test "Option.required with TsDecode.int has integer schema" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.required "count" TsDecode.int)
                            )
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$cli", Encode.string "elm-cli-options-parser" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "count", Encode.object [ ( "type", Encode.string "integer" ) ] )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "count" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "Option.required CLI mode parses string via decodeString" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.required "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "--count", "42" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch 42)
            , test "Option.required JSON mode decodes native value" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.required "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"count\":42}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch 42)
            , test "Option.required CLI mode error for non-numeric string" <|
                \() ->
                    -- "abc" should produce "Expecting an INT", not "not valid JSON"
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.required "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "--count", "abc" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> expectRunResultContains "Expecting an INT"
            , test "Option.required CLI mode error for wrong JSON type" <|
                \() ->
                    -- "true" is valid JSON but not an int
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.required "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "--count", "true" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> expectRunResultContains "Expecting an INT"
            , test "Option.required with string decoder works in CLI mode without quoting" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.required "name" TsDecode.string)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "--name", "hello" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch "hello")
            , test "Option.required with description includes it in schema" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with
                                    (Option.required "count" TsDecode.int
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
            , test "Option.optional with TsDecode.int has integer schema and is not required" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.optional "count" TsDecode.int)
                            )
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$cli", Encode.string "elm-cli-options-parser" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "count", Encode.object [ ( "type", Encode.string "integer" ) ] )
                                        ]
                                  )
                                ]
                                |> Encode.encode 0
                            )
            , test "Option.optional JSON mode decodes native value" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.optional "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"count\":42}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch (Just 42))
            , test "Option.optional JSON mode returns Nothing when absent" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.optional "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\"}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch Nothing)
            , test "Option.optional CLI mode parses string via decodeString" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.optional "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "--count", "42" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch (Just 42))
            , test "Option.optional CLI mode returns Nothing when absent" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.optional "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch Nothing)
            , test "Option.required JSON mode error for wrong type" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.required "count" TsDecode.int)
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"count\":\"not-a-number\"}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> expectRunResultContains "Expecting an INT"
            ]
        , describe "JSON input mode"
            [ test "accepts JSON blob with $cli sentinel" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build (\name greeting -> { name = name, greeting = greeting })
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"name\":\"World\",\"greeting\":\"Hi\"}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch { name = "World", greeting = Just "Hi" })
            , test "JSON input mode with boolean flag" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build (\name verbose -> { name = name, verbose = verbose })
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                                |> OptionsParser.with (Option.flag "verbose")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"name\":\"World\",\"verbose\":true}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch { name = "World", verbose = True })
            , test "JSON input mode with subcommand" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.buildSubCommand "greet" identity
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"subcommand\":\"greet\",\"name\":\"World\"}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch "World")
            , test "JSON input mode with typed JSON arg" <|
                \() ->
                    let
                        todoDecoder =
                            TsDecode.map2 (\title desc -> { title = title, description = desc })
                                (TsDecode.field "title" TsDecode.string)
                                (TsDecode.field "description" TsDecode.string)
                    in
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with
                                    (Option.requiredKeywordArg "todo"
                                        |> Option.withTypedJson todoDecoder
                                    )
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"todo\":{\"title\":\"Buy groceries\",\"description\":\"Get milk\"}}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch { title = "Buy groceries", description = "Get milk" })
                , test "JSON input mode with missing required field gives JSON-native error" <|
                \() ->
                    let
                        cfg =
                            Program.config
                                |> Program.add
                                    (OptionsParser.build (\name greeting -> { name = name, greeting = greeting })
                                        |> OptionsParser.with (Option.requiredKeywordArg "name")
                                        |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                                    )
                    in
                    Program.run cfg
                        [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"greeting\":\"Hi\"}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                "Missing required field: \"name\""
                            )
            , test "JSON input mode with invalid JSON for typed arg gives validation error" <|
                \() ->
                    let
                        todoDecoder =
                            TsDecode.map2 (\title desc -> { title = title, description = desc })
                                (TsDecode.field "title" TsDecode.string)
                                (TsDecode.field "description" TsDecode.string)

                        result =
                            Program.config
                                |> Program.add
                                    (OptionsParser.build identity
                                        |> OptionsParser.with
                                            (Option.requiredKeywordArg "todo"
                                                |> Option.withTypedJson todoDecoder
                                            )
                                    )
                                |> (\cfg ->
                                        Program.run cfg
                                            [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"todo\":{\"title\":123}}" ]
                                            "1.0.0"
                                            Program.WithoutColor
                                   )
                    in
                    Expect.equal result
                        (Program.SystemMessage Program.Failure
                            """Validation errors:

Invalid "todo" field.
Problem with the value at json.todo.title:

    123

Expecting a STRING"""
                        )
            , test "traditional CLI with invalid JSON for typed arg gives same validation error" <|
                \() ->
                    let
                        todoDecoder =
                            TsDecode.map2 (\title desc -> { title = title, description = desc })
                                (TsDecode.field "title" TsDecode.string)
                                (TsDecode.field "description" TsDecode.string)

                        result =
                            Program.config
                                |> Program.add
                                    (OptionsParser.build identity
                                        |> OptionsParser.with
                                            (Option.requiredKeywordArg "todo"
                                                |> Option.withTypedJson todoDecoder
                                            )
                                    )
                                |> (\cfg ->
                                        Program.run cfg
                                            [ "node", "test", "--todo", "{\"title\":123}" ]
                                            "1.0.0"
                                            Program.WithoutColor
                                   )
                    in
                    Expect.equal result
                        (Program.SystemMessage Program.Failure
                            """Validation errors:

Invalid `--todo` option.
Problem with the value at json.title:

    123

Expecting a STRING"""
                        )
            , test "JSON input mode with wrong type for untyped arg gives type error" <|
                \() ->
                    -- With direct JSON decoding, number 123 for a string field is a type error.
                    -- No more silent number-to-string coercion.
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":\"elm-cli-options-parser\",\"name\":123}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid "name" field.
Problem with the value at json.name:

    123

Expecting a STRING"""
                            )
            , test "malformed JSON falls back to regular CLI parsing" <|
                \() ->
                    -- Malformed JSON is NOT treated as JSON input mode,
                    -- it falls back to regular CLI parsing where it becomes a positional arg
                    let
                        result =
                            Program.config
                                |> Program.add
                                    (OptionsParser.build identity
                                        |> OptionsParser.with (Option.requiredPositionalArg "input")
                                    )
                                |> (\cfg ->
                                        Program.run cfg
                                            [ "node", "test", "{not valid json" ]
                                            "1.0.0"
                                            Program.WithoutColor
                                   )
                    in
                    Expect.equal result (Program.CustomMatch "{not valid json")
            ]
        ]


{-| Helper to build expected JSON Schema and compare.
-}
expectJsonSchema :
    { properties : List ( String, List ( String, Encode.Value ) )
    , required : List String
    }
    -> Program.Config msg
    -> Expect.Expectation
expectJsonSchema { properties, required } config =
    config
        |> Program.toJsonSchema
        |> Encode.encode 0
        |> Expect.equal
            (Encode.object
                ([ ( "$cli", Encode.string "elm-cli-options-parser" )
                 , ( "type", Encode.string "object" )
                 , ( "properties"
                   , Encode.object
                        (properties
                            |> List.map (\( name, fields ) -> ( name, Encode.object fields ))
                        )
                   )
                 ]
                    ++ (if List.isEmpty required then
                            []

                        else
                            [ ( "required", Encode.list Encode.string required ) ]
                       )
                )
                |> Encode.encode 0
            )


{-| Assert that a Program.RunResult contains a specific substring in its error message.
-}
expectRunResultContains : String -> Program.RunResult msg -> Expect.Expectation
expectRunResultContains substring result =
    case result of
        Program.SystemMessage Program.Failure message ->
            if String.contains substring message then
                Expect.pass

            else
                Expect.fail ("Expected error containing \"" ++ substring ++ "\" but got:\n" ++ message)

        other ->
            Expect.fail ("Expected SystemMessage Failure but got: " ++ Debug.toString other)
