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
                            { description = "test --name <NAME>"
                            , properties =
                                [ ( "name", [ ( "type", Encode.string "string" ) ] ) ]
                            , required = [ "name" ]
                            }
            , test "schema forbids additional top-level and $cli properties" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                            )
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test --name <NAME>" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "name"
                                          , Encode.object
                                                [ ( "type", Encode.string "string" )
                                                , ( "x-cli-kind", Encode.string "keyword" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "name", "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
                                ]
                                |> Encode.encode 0
                            )
            , test "optional keyword arg is not required" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                            )
                        |> expectJsonSchema
                            { description = "test [--greeting <GREETING>]"
                            , properties =
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test [--verbose]" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "verbose"
                                          , Encode.object
                                                [ ( "type", Encode.string "boolean" )
                                                , ( "x-cli-kind", Encode.string "flag" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test <file>" True) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "positional"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "array" )
                                                                , ( "description", Encode.string "Positional arguments, passed in order (e.g., mytool <source> <dest>)" )
                                                                , ( "prefixItems"
                                                                  , Encode.list identity
                                                                        [ Encode.object [ ( "type", Encode.string "string" ), ( "description", Encode.string "file" ) ] ]
                                                                  )
                                                                , ( "items", Encode.bool False )
                                                                , ( "minItems", Encode.int 1 )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "positional" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test [<revision>]" True) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "positional"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "array" )
                                                                , ( "description", Encode.string "Positional arguments, passed in order (e.g., mytool <source> <dest>)" )
                                                                , ( "prefixItems"
                                                                  , Encode.list identity
                                                                        [ Encode.object [ ( "type", Encode.string "string" ), ( "description", Encode.string "revision" ) ] ]
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
                                , ( "additionalProperties", Encode.bool False )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test <files>..." True) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
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
                                , ( "additionalProperties", Encode.bool False )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test [--header <HEADER>]..." False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "header"
                                          , Encode.object
                                                [ ( "type", Encode.string "array" )
                                                , ( "items", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                , ( "x-cli-kind", Encode.string "keyword-list" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
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
                            { description = "test --name <NAME>"
                            , properties =
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test --format <json|junit|console>" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "format"
                                          , Encode.object
                                                [ ( "type", Encode.string "string" )
                                                , ( "enum", Encode.list Encode.string [ "json", "junit", "console" ] )
                                                , ( "x-cli-kind", Encode.string "keyword" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "format", "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test --name <NAME> [--greeting <GREETING>] [--verbose]" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "name"
                                          , Encode.object
                                                [ ( "type", Encode.string "string" )
                                                , ( "x-cli-kind", Encode.string "keyword" )
                                                ]
                                          )
                                        , ( "greeting"
                                          , Encode.object
                                                [ ( "type", Encode.string "string" )
                                                , ( "x-cli-kind", Encode.string "keyword" )
                                                ]
                                          )
                                        , ( "verbose"
                                          , Encode.object
                                                [ ( "type", Encode.string "boolean" )
                                                , ( "x-cli-kind", Encode.string "flag" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "name", "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
                                ]
                                |> Encode.encode 0
                            )
            , test "expectFlag produces required constraint in schema" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build ()
                                |> OptionsParser.expectFlag "init"
                            )
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test --init" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "init"
                                          , Encode.object
                                                [ ( "type", Encode.string "boolean" )
                                                , ( "x-cli-kind", Encode.string "flag" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "init", "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
                                ]
                                |> Encode.encode 0
                            )
            , test "multiple expectFlags produce required on flags object" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build ()
                                |> OptionsParser.expectFlag "init"
                                |> OptionsParser.expectFlag "force"
                            )
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test --init --force" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "init"
                                          , Encode.object
                                                [ ( "type", Encode.string "boolean" )
                                                , ( "x-cli-kind", Encode.string "flag" )
                                                ]
                                          )
                                        , ( "force"
                                          , Encode.object
                                                [ ( "type", Encode.string "boolean" )
                                                , ( "x-cli-kind", Encode.string "flag" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "init", "force", "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
                                ]
                                |> Encode.encode 0
                            )
            , test "mixed flag and expectFlag — only expectFlag gets required" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.flag "verbose")
                                |> OptionsParser.expectFlag "init"
                            )
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test [--verbose] --init" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "verbose"
                                          , Encode.object
                                                [ ( "type", Encode.string "boolean" )
                                                , ( "x-cli-kind", Encode.string "flag" )
                                                ]
                                          )
                                        , ( "init"
                                          , Encode.object
                                                [ ( "type", Encode.string "boolean" )
                                                , ( "x-cli-kind", Encode.string "flag" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "init", "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
                                ]
                                |> Encode.encode 0
                            )
            , test "discriminated union with expectFlag produces anyOf with required flags" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.expectFlag "init"
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                                |> OptionsParser.map (\_ -> ())
                            )
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.expectFlag "build"
                                |> OptionsParser.with (Option.flag "verbose")
                                |> OptionsParser.map (\_ -> ())
                            )
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "anyOf"
                                  , Encode.list identity
                                        [ Encode.object
                                            [ ( "description", Encode.string (fullDescription "test --init --name <NAME>" False) )
                                            , ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "init"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "boolean" )
                                                            , ( "x-cli-kind", Encode.string "flag" )
                                                            ]
                                                      )
                                                    , ( "name"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "string" )
                                                            , ( "x-cli-kind", Encode.string "keyword" )
                                                            ]
                                                      )
                                                    , ( "$cli"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "object" )
                                                            , ( "additionalProperties", Encode.bool False )
                                                            ]
                                                      )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "init", "name", "$cli" ] )
                                            , ( "additionalProperties", Encode.bool False )
                                            ]
                                        , Encode.object
                                            [ ( "description", Encode.string (fullDescription "test --build [--verbose]" False) )
                                            , ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "build"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "boolean" )
                                                            , ( "x-cli-kind", Encode.string "flag" )
                                                            ]
                                                      )
                                                    , ( "verbose"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "boolean" )
                                                            , ( "x-cli-kind", Encode.string "flag" )
                                                            ]
                                                      )
                                                    , ( "$cli"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "object" )
                                                            , ( "additionalProperties", Encode.bool False )
                                                            ]
                                                      )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "build", "$cli" ] )
                                            , ( "additionalProperties", Encode.bool False )
                                            ]
                                        ]
                                  )
                                ]
                                |> Encode.encode 0
                            )
            , test "no options produces empty object schema" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build ())
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string (fullDescription "test init [--bare]" False) )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "bare"
                                          , Encode.object
                                                [ ( "type", Encode.string "boolean" )
                                                , ( "x-cli-kind", Encode.string "flag" )
                                                ]
                                          )
                                        , ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "additionalProperties", Encode.bool False )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "init" ) ] )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "subcommand" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
                                , ( "additionalProperties", Encode.bool False )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "anyOf"
                                  , Encode.list identity
                                        [ Encode.object
                                            [ ( "description", Encode.string (fullDescription "test init" False) )
                                            , ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "$cli"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "object" )
                                                            , ( "additionalProperties", Encode.bool False )
                                                            , ( "properties"
                                                              , Encode.object
                                                                    [ ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "init" ) ] )
                                                                    ]
                                                              )
                                                            , ( "required", Encode.list Encode.string [ "subcommand" ] )
                                                            ]
                                                      )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "$cli" ] )
                                            , ( "additionalProperties", Encode.bool False )
                                            ]
                                        , Encode.object
                                            [ ( "description", Encode.string (fullDescription "test clone <repository>" True) )
                                            , ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "$cli"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "object" )
                                                            , ( "additionalProperties", Encode.bool False )
                                                            , ( "properties"
                                                              , Encode.object
                                                                    [ ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "clone" ) ] )
                                                                    , ( "positional"
                                                                      , Encode.object
                                                                            [ ( "type", Encode.string "array" )
                                                                            , ( "description", Encode.string "Positional arguments, passed in order (e.g., mytool <source> <dest>)" )
                                                                            , ( "prefixItems"
                                                                              , Encode.list identity
                                                                                    [ Encode.object [ ( "type", Encode.string "string" ), ( "description", Encode.string "repository" ) ] ]
                                                                              )
                                                                            , ( "items", Encode.bool False )
                                                                            , ( "minItems", Encode.int 1 )
                                                                            ]
                                                                      )
                                                                    ]
                                                              )
                                                            , ( "required", Encode.list Encode.string [ "subcommand", "positional" ] )
                                                            ]
                                                      )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "$cli" ] )
                                            , ( "additionalProperties", Encode.bool False )
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
                                    [ "node", "test", "{\"name\":\"World\",\"greeting\":\"Hi\",\"$cli\":{}}" ]
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
                                    [ "node", "test", "{\"name\":\"World\",\"verbose\":true,\"$cli\":{}}" ]
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
                                    [ "node", "test", "{\"name\":\"World\",\"$cli\":{\"subcommand\":\"greet\"}}" ]
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
                        [ "node", "test", "{\"greeting\":\"Hi\",\"$cli\":{}}" ]
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
                                    [ "node", "test", "{\"name\":123,\"$cli\":{}}" ]
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
            , test "JSON input mode rejects unexpected top-level field" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"name\":\"World\",\"nickname\":\"W\",\"$cli\":{}}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                "Unexpected field: \"nickname\""
                            )
            , test "JSON input mode rejects unexpected $cli field" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredKeywordArg "name")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"name\":\"World\",\"$cli\":{\"mode\":\"json\"}}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                "Unexpected field: \"$cli.mode\""
                            )
            , test "JSON input mode rejects extra positional values" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build identity
                                |> OptionsParser.with (Option.requiredPositionalArg "file")
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":{\"positional\":[\"a.txt\",\"b.txt\"]}}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                "Too many positional arguments in \"$cli.positional\"."
                            )
            , test "JSON input mode only reports fields as unexpected when no parser accepts them" <|
                \() ->
                    let
                        cfg =
                            Program.config
                                |> Program.add
                                    (OptionsParser.build (\name -> "init:" ++ name)
                                        |> OptionsParser.expectFlag "init"
                                        |> OptionsParser.with (Option.requiredKeywordArg "name")
                                    )
                                |> Program.add
                                    (OptionsParser.build "build"
                                        |> OptionsParser.expectFlag "build"
                                    )
                    in
                    Program.run cfg
                        [ "node", "test", "{\"init\":true,\"$cli\":{}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                "Missing required field: \"name\""
                            )
            , test "JSON input mode expectFlag selects init branch" <|
                \() ->
                    let
                        cfg =
                            Program.config
                                |> Program.add
                                    (OptionsParser.build (\name -> "init:" ++ name)
                                        |> OptionsParser.expectFlag "init"
                                        |> OptionsParser.with (Option.requiredKeywordArg "name")
                                    )
                                |> Program.add
                                    (OptionsParser.build
                                        (\verbose ->
                                            "build:"
                                                ++ (if verbose then
                                                        "verbose"

                                                    else
                                                        "quiet"
                                                   )
                                        )
                                        |> OptionsParser.expectFlag "build"
                                        |> OptionsParser.with (Option.flag "verbose")
                                    )
                    in
                    Program.run cfg
                        [ "node", "test", "{\"init\":true,\"name\":\"my-project\",\"$cli\":{}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch "init:my-project")
            , test "JSON input mode expectFlag selects build branch" <|
                \() ->
                    let
                        cfg =
                            Program.config
                                |> Program.add
                                    (OptionsParser.build (\name -> "init:" ++ name)
                                        |> OptionsParser.expectFlag "init"
                                        |> OptionsParser.with (Option.requiredKeywordArg "name")
                                    )
                                |> Program.add
                                    (OptionsParser.build
                                        (\verbose ->
                                            "build:"
                                                ++ (if verbose then
                                                        "verbose"

                                                    else
                                                        "quiet"
                                                   )
                                        )
                                        |> OptionsParser.expectFlag "build"
                                        |> OptionsParser.with (Option.flag "verbose")
                                    )
                    in
                    Program.run cfg
                        [ "node", "test", "{\"build\":true,\"verbose\":true,\"$cli\":{}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch "build:verbose")
            , test "JSON input mode expectFlag rejects when flag missing" <|
                \() ->
                    Program.config
                        |> Program.add
                            (OptionsParser.build ()
                                |> OptionsParser.expectFlag "init"
                            )
                        |> (\cfg ->
                                Program.run cfg
                                    [ "node", "test", "{\"$cli\":{}}" ]
                                    "1.0.0"
                                    Program.WithoutColor
                           )
                        |> (\result ->
                                case result of
                                    Program.SystemMessage Program.Failure _ ->
                                        Expect.pass

                                    _ ->
                                        Expect.fail ("Expected failure but got: " ++ Debug.toString result)
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


{-| Build the full description with invocation instructions, matching the source code's
`buildSchemaDescription` function.
-}
fullDescription : String -> Bool -> String
fullDescription synopsis hasPositionalArgs =
    let
        positionalNote =
            if hasPositionalArgs then
                "Positional arguments are passed in order via the `$cli.positional` array."

            else
                "Positional arguments are passed in order via the `$cli.positional` array (for this CLI it will always be empty)."
    in
    synopsis
        ++ "\n\nTo invoke this command, build a JSON object matching this schema and pass it as a single argument. Alternatively, use traditional CLI flags as shown in the usage line above."
        ++ "\n\nEach property has an `x-cli-kind` indicating its CLI invocation form:\n- \"keyword\": --name <value>\n- \"flag\": --name (present or absent, no value)\n- \"keyword-list\": --name <value> (repeatable)"
        ++ "\n\n"
        ++ positionalNote


{-| Helper to build expected JSON Schema and compare.
Used for tests where only keyword args are present (no flags, positional args,
or keyword arg lists). Places keyword args as flat top-level properties with
`x-cli-kind: "keyword"` and a `$cli: {"type": "object"}`.
-}
expectJsonSchema :
    { description : String
    , properties : List ( String, List ( String, Encode.Value ) )
    , required : List String
    }
    -> Program.Config msg
    -> Expect.Expectation
expectJsonSchema { description, properties, required } config =
    let
        topLevelProperties =
            properties
                |> List.map
                    (\( name, fields ) ->
                        let
                            -- Separate base type fields from description
                            baseFields =
                                fields |> List.filter (\( k, _ ) -> k /= "description")

                            descFields =
                                fields |> List.filter (\( k, _ ) -> k == "description")
                        in
                        ( name
                        , Encode.object
                            (baseFields ++ [ ( "x-cli-kind", Encode.string "keyword" ) ] ++ descFields)
                        )
                    )

        cliObj =
            Encode.object
                [ ( "type", Encode.string "object" )
                , ( "additionalProperties", Encode.bool False )
                ]

        allProperties =
            topLevelProperties ++ [ ( "$cli", cliObj ) ]

        allRequired =
            required ++ [ "$cli" ]
    in
    config
        |> Program.toJsonSchema "test"
        |> Encode.encode 0
        |> Expect.equal
            (Encode.object
                [ ( "description", Encode.string (fullDescription description False) )
                , ( "type", Encode.string "object" )
                , ( "properties", Encode.object allProperties )
                , ( "required", Encode.list Encode.string allRequired )
                , ( "additionalProperties", Encode.bool False )
                ]
                |> Encode.encode 0
            )
