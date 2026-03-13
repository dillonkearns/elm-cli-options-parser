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
                            { description = "Usage: test --name <NAME>"
                            , properties =
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
                            { description = "Usage: test [--greeting <GREETING>]"
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
                                [ ( "description", Encode.string "Usage: test [--verbose]" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "verbose", Encode.object [ ( "type", Encode.string "boolean" ) ] ) ]
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string "Usage: test <file>" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string "Usage: test [<revision>]" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
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
                                [ ( "description", Encode.string "Usage: test <files>..." )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string "Usage: test [--header <HEADER>]..." )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
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
                            { description = "Usage: test --name <NAME>\n\nOptions:\n  --name <NAME>   The user's name"
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
                                [ ( "description", Encode.string "Usage: test --format <json|junit|console>" )
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
                                                                        [ ( "format"
                                                                          , Encode.object
                                                                                [ ( "type", Encode.string "string" )
                                                                                , ( "enum"
                                                                                  , Encode.list Encode.string [ "json", "junit", "console" ]
                                                                                  )
                                                                                ]
                                                                          )
                                                                        ]
                                                                  )
                                                                , ( "required", Encode.list Encode.string [ "format" ] )
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
                                [ ( "description", Encode.string "Usage: test --name <NAME> [--greeting <GREETING>] [--verbose]" )
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
                                                                        [ ( "name", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                                        , ( "greeting", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                                        ]
                                                                  )
                                                                , ( "required", Encode.list Encode.string [ "name" ] )
                                                                ]
                                                          )
                                                        , ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "verbose", Encode.object [ ( "type", Encode.string "boolean" ) ] ) ]
                                                                  )
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
                                [ ( "description", Encode.string "Usage: test --init" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "init", Encode.object [ ( "type", Encode.string "boolean" ) ] ) ]
                                                                  )
                                                                , ( "required", Encode.list Encode.string [ "init" ] )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "flags" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
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
                                [ ( "description", Encode.string "Usage: test --init --force" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "init", Encode.object [ ( "type", Encode.string "boolean" ) ] )
                                                                        , ( "force", Encode.object [ ( "type", Encode.string "boolean" ) ] )
                                                                        ]
                                                                  )
                                                                , ( "required", Encode.list Encode.string [ "init", "force" ] )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "flags" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
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
                                [ ( "description", Encode.string "Usage: test [--verbose] --init" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "verbose", Encode.object [ ( "type", Encode.string "boolean" ) ] )
                                                                        , ( "init", Encode.object [ ( "type", Encode.string "boolean" ) ] )
                                                                        ]
                                                                  )
                                                                , ( "required", Encode.list Encode.string [ "init" ] )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "flags" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
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
                                            [ ( "description", Encode.string "Usage: test --init --name <NAME>" )
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
                                                                    , ( "flags"
                                                                      , Encode.object
                                                                            [ ( "type", Encode.string "object" )
                                                                            , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                            , ( "properties"
                                                                              , Encode.object
                                                                                    [ ( "init", Encode.object [ ( "type", Encode.string "boolean" ) ] ) ]
                                                                              )
                                                                            , ( "required", Encode.list Encode.string [ "init" ] )
                                                                            ]
                                                                      )
                                                                    ]
                                                              )
                                                            , ( "required", Encode.list Encode.string [ "keywordValues", "flags" ] )
                                                            ]
                                                      )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "$cli" ] )
                                            ]
                                        , Encode.object
                                            [ ( "description", Encode.string "Usage: test --build [--verbose]" )
                                            , ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "$cli"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "object" )
                                                            , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                            , ( "properties"
                                                              , Encode.object
                                                                    [ ( "flags"
                                                                      , Encode.object
                                                                            [ ( "type", Encode.string "object" )
                                                                            , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                            , ( "properties"
                                                                              , Encode.object
                                                                                    [ ( "build", Encode.object [ ( "type", Encode.string "boolean" ) ] )
                                                                                    , ( "verbose", Encode.object [ ( "type", Encode.string "boolean" ) ] )
                                                                                    ]
                                                                              )
                                                                            , ( "required", Encode.list Encode.string [ "build" ] )
                                                                            ]
                                                                      )
                                                                    ]
                                                              )
                                                            , ( "required", Encode.list Encode.string [ "flags" ] )
                                                            ]
                                                      )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "$cli" ] )
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
                                [ ( "description", Encode.string "Usage: test" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli", Encode.object [ ( "type", Encode.string "object" ), ( "description", Encode.string "Required CLI input object. Include as empty object {} when no arguments are needed." ) ] )
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
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string "Usage: test init [--bare]" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "subcommand", Encode.object [ ( "type", Encode.string "string" ), ( "const", Encode.string "init" ) ] )
                                                        , ( "flags"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "object" )
                                                                , ( "description", Encode.string "Boolean flags, passed as --flag (e.g., --verbose)" )
                                                                , ( "properties"
                                                                  , Encode.object
                                                                        [ ( "bare", Encode.object [ ( "type", Encode.string "boolean" ) ] ) ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "subcommand" ] )
                                                ]
                                          )
                                        ]
                                  )
                                , ( "required", Encode.list Encode.string [ "$cli" ] )
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
                                            [ ( "description", Encode.string "Usage: test init" )
                                            , ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "$cli"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "object" )
                                                            , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
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
                                            ]
                                        , Encode.object
                                            [ ( "description", Encode.string "Usage: test clone <repository>" )
                                            , ( "type", Encode.string "object" )
                                            , ( "properties"
                                              , Encode.object
                                                    [ ( "$cli"
                                                      , Encode.object
                                                            [ ( "type", Encode.string "object" )
                                                            , ( "description", Encode.string "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable." )
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
                                                            , ( "required", Encode.list Encode.string [ "subcommand" ] )
                                                            ]
                                                      )
                                                    ]
                                              )
                                            , ( "required", Encode.list Encode.string [ "$cli" ] )
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
                                    [ "node", "test", "{\"$cli\":{\"keywordValues\":{\"name\":\"World\",\"greeting\":\"Hi\"}}}" ]
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
                                    [ "node", "test", "{\"$cli\":{\"flags\":{\"verbose\":true},\"keywordValues\":{\"name\":\"World\"}}}" ]
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
                                    [ "node", "test", "{\"$cli\":{\"subcommand\":\"greet\",\"keywordValues\":{\"name\":\"World\"}}}" ]
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
                        [ "node", "test", "{\"$cli\":{\"keywordValues\":{\"greeting\":\"Hi\"}}}" ]
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
                                    [ "node", "test", "{\"$cli\":{\"keywordValues\":{\"name\":123}}}" ]
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
                        [ "node", "test", "{\"$cli\":{\"flags\":{\"init\":true},\"keywordValues\":{\"name\":\"my-project\"}}}" ]
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
                        [ "node", "test", "{\"$cli\":{\"flags\":{\"build\":true,\"verbose\":true}}}" ]
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


{-| Helper to build expected JSON Schema and compare.
Used for tests where only keyword args are present (no flags, positional args,
or keyword arg lists). Nests keyword args under `$cli.keywordValues`.
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
        keywordValuesObj =
            Encode.object
                ([ ( "type", Encode.string "object" )
                 , ( "description", Encode.string "Keyword arguments with values (e.g., --name <value>)" )
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

        cliSubProperties =
            if List.isEmpty properties then
                []

            else
                [ ( "keywordValues", keywordValuesObj ) ]

        cliRequired =
            if List.isEmpty required then
                []

            else
                [ "keywordValues" ]

        cliDescription =
            if List.isEmpty cliSubProperties then
                "Required CLI input object. Include as empty object {} when no arguments are needed."

            else
                "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable."

        cliObj =
            Encode.object
                ([ ( "type", Encode.string "object" )
                 , ( "description", Encode.string cliDescription )
                 ]
                    ++ (if List.isEmpty cliSubProperties then
                            []

                        else
                            [ ( "properties", Encode.object cliSubProperties ) ]
                       )
                    ++ (if List.isEmpty cliRequired then
                            []

                        else
                            [ ( "required", Encode.list Encode.string cliRequired ) ]
                       )
                )
    in
    config
        |> Program.toJsonSchema "test"
        |> Encode.encode 0
        |> Expect.equal
            (Encode.object
                [ ( "description", Encode.string description )
                , ( "type", Encode.string "object" )
                , ( "properties"
                  , Encode.object
                        [ ( "$cli", cliObj ) ]
                  )
                , ( "required", Encode.list Encode.string [ "$cli" ] )
                ]
                |> Encode.encode 0
            )
