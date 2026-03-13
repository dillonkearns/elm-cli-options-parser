module JsonSchemaTests exposing (all)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Expect
import Json.Encode as Encode
import Test exposing (..)


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
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "array" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "items"
                                                                  , Encode.object
                                                                        [ ( "enum", Encode.list Encode.string [ "verbose" ] ) ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "required positional arg" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredPositionalArg "file")
                            )
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "positional"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "array" )
                                                                , ( "description", Encode.string "Positional arguments, passed in order (e.g., mytool <source> <dest>)" )
                                                                , ( "prefixItems"
                                                                  , Encode.list identity
                                                                        [ Encode.object [ ( "type", Encode.string "string" ) ] ]
                                                                  )
                                                                , ( "items", Encode.bool False )
                                                                , ( "minItems", Encode.int 1 )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "optional positional arg" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.withOptionalPositionalArg (Option.optionalPositionalArg "revision")
                            )
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "positional"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "array" )
                                                                , ( "description", Encode.string "Positional arguments, passed in order (e.g., mytool <source> <dest>)" )
                                                                , ( "prefixItems"
                                                                  , Encode.list identity
                                                                        [ Encode.object [ ( "type", Encode.string "string" ) ] ]
                                                                  )
                                                                , ( "items", Encode.bool False )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "rest args is array of strings" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.withRestArgs (Option.restArgs "files")
                            )
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "positional"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "array" )
                                                                , ( "description", Encode.string "Positional arguments, passed in order (e.g., mytool <source> <dest>)" )
                                                                , ( "items", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "keyword arg list is array of strings" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.keywordArgList "header")
                            )
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "keywordLists"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Keyword arguments that can be repeated (e.g., --header X --header Y)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "header"
                                                                          , Encode.object
                                                                                [ ( "type", Encode.string "array" )
                                                                                , ( "items", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                                                ]
                                                                          )
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
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
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli", Encode.object [ ( "type", Encode.string "object" ) ] )
                                        , ( "format"
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
                                , ( "required", Encode.list Encode.string [ "$cli", "format" ] )
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
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "array" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "items"
                                                                  , Encode.object
                                                                        [ ( "enum", Encode.list Encode.string [ "verbose" ] ) ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        , ( "name", Encode.object [ ( "type", Encode.string "string" ) ] )
                                        , ( "greeting", Encode.object [ ( "type", Encode.string "string" ) ] )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli", "name" ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "no options produces empty object schema" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build ())
                        |> Program.toJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli", Encode.object [ ( "type", Encode.string "object" ) ] )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                ]
                                |> Encode.encode 0
                            )
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
                                [ ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "array" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "items"
                                                                  , Encode.object
                                                                        [ ( "enum", Encode.list Encode.string [ "bare" ] ) ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        , ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "init" ) ] )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli", "subcommand" ] )
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
                                [ ( "anyOf"
                                  , Encode.list identity
                                        [ Encode.object
                                            [ ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "$cli", Encode.object [ ( "type", Encode.string "object" ) ] )
                                                    , ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "init" ) ] )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "$cli", "subcommand" ] )
                                            ]
                                        , Encode.object
                                            [ ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "$cli"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "object" )
                                                            , ( "properties"
                                                              , Encode.object
                                                                    [ ( "positional"
                                                                      , Encode.object
                                                                            [ ( "type", Encode.string "array" )
                                                                            , ( "description", Encode.string "Positional arguments, passed in order (e.g., mytool <source> <dest>)" )
                                                                            , ( "prefixItems"
                                                                              , Encode.list identity
                                                                                    [ Encode.object [ ( "type", Encode.string "string" ) ] ]
                                                                              )
                                                                            , ( "items", Encode.bool False )
                                                                            , ( "minItems", Encode.int 1 )
                                                                            ]
                                                                      )
                                                                    ]
                                                              )
                                                            ]
                                                      )
                                                    , ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "clone" ) ] )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "$cli", "subcommand" ] )
                                            ]
                                        ]
                                  )
                                ]
                                |> Encode.encode 0
                            )
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
                                    [ "node", "test", "{\"$cli\":{},\"name\":\"World\",\"greeting\":\"Hi\"}" ]
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
                                    [ "node", "test", "{\"$cli\":{\"flags\":[\"verbose\"]},\"name\":\"World\"}" ]
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
                                    [ "node", "test", "{\"$cli\":{},\"subcommand\":\"greet\",\"name\":\"World\"}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.CustomMatch "World")
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
                        [ "node", "test", "{\"$cli\":{},\"greeting\":\"Hi\"}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                "Missing required field: \"name\""
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
                                    [ "node", "test", "{\"$cli\":{},\"name\":123}" ]
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
Used for tests where only keyword args are present (no flags, positional args,
or keyword arg lists). Adds `$cli` as a `{"type": "object"}` property and
always includes `$cli` in `required`.
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
                [ ( "type", Encode.string "object" )
                , ( "properties"
                  , Encode.object
                        (( "$cli", Encode.object [ ( "type", Encode.string "object" ) ] )
                            :: (properties
                                    |> List.map (\( name, fields ) -> ( name, Encode.object fields ))
                               )
                        )
                  )
                , ( "required", Encode.list Encode.string ("$cli" :: required) )
                ]
                |> Encode.encode 0
            )
