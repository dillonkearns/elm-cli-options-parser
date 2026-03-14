module TsTypeTests exposing (all)

import Cli.Option as Option
import Cli.Option.Internal as Internal
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Expect
import Json.Encode as Encode
import Test exposing (..)
import TsJson.Type


{-| Extract the TsType from an option and convert to JSON Schema for testing.
-}
optionTsTypeToJsonSchema : Internal.Option from to constraints -> Encode.Value
optionTsTypeToJsonSchema (Internal.Option innerOption) =
    TsJson.Type.toJsonSchema innerOption.tsType


all : Test
all =
    describe "Option TsType"
        [ describe "basic option constructors carry correct TsType"
            [ test "requiredKeywordArg has string type" <|
                \() ->
                    Option.requiredKeywordArg "name"
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "string" )
                                ]
                                |> Encode.encode 0
                            )
            , test "optionalKeywordArg has string type (optionality expressed via required array)" <|
                \() ->
                    Option.optionalKeywordArg "greeting"
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "string" )
                                ]
                                |> Encode.encode 0
                            )
            , test "flag has boolean type" <|
                \() ->
                    Option.flag "verbose"
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "boolean" )
                                ]
                                |> Encode.encode 0
                            )
            , test "requiredPositionalArg has string type" <|
                \() ->
                    Option.requiredPositionalArg "file"
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "string" )
                                ]
                                |> Encode.encode 0
                            )
            , test "optionalPositionalArg has string type (optionality expressed via required array)" <|
                \() ->
                    Option.optionalPositionalArg "revision"
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "string" )
                                ]
                                |> Encode.encode 0
                            )
            , test "keywordArgList has array of strings type" <|
                \() ->
                    Option.keywordArgList "header"
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "array" )
                                , ( "items", Encode.object [ ( "type", Encode.string "string" ) ] )
                                ]
                                |> Encode.encode 0
                            )
            , test "restArgs has array of strings type" <|
                \() ->
                    Option.restArgs "files"
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "array" )
                                , ( "items", Encode.object [ ( "type", Encode.string "string" ) ] )
                                ]
                                |> Encode.encode 0
                            )
            ]
        , describe "modifiers preserve TsType"
            [ test "map preserves TsType" <|
                \() ->
                    Option.requiredKeywordArg "name"
                        |> Option.map String.toUpper
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "string" )
                                ]
                                |> Encode.encode 0
                            )
            , test "validateMap preserves TsType" <|
                \() ->
                    Option.requiredKeywordArg "count"
                        |> Option.validateMap
                            (\s ->
                                case String.toInt s of
                                    Just n ->
                                        Ok n

                                    Nothing ->
                                        Err "not an int"
                            )
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "string" )
                                ]
                                |> Encode.encode 0
                            )
            , test "withDefault preserves TsType" <|
                \() ->
                    Option.optionalKeywordArg "greeting"
                        |> Option.withDefault "hello"
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "string" )
                                ]
                                |> Encode.encode 0
                            )
            , test "mapFlag preserves TsType" <|
                \() ->
                    Option.flag "verbose"
                        |> Option.mapFlag { present = "yes", absent = "no" }
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "boolean" )
                                ]
                                |> Encode.encode 0
                            )
            ]
        , describe "oneOf updates TsType"
            [ test "oneOf on requiredKeywordArg produces string literal union type" <|
                \() ->
                    Option.requiredKeywordArg "format"
                        |> Option.oneOf
                            [ ( "json", () )
                            , ( "junit", () )
                            , ( "console", () )
                            ]
                        |> optionTsTypeToJsonSchema
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
                                , ( "type", Encode.string "string" )
                                , ( "enum"
                                  , Encode.list Encode.string [ "json", "junit", "console" ]
                                  )
                                ]
                                |> Encode.encode 0
                            )
            ]
        , describe "toJsonSchema output"
            [ test "oneOf uses anyOf/const format" <|
                \() ->
                    let
                        cfg =
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

                        desc =
                            "test --format <json|junit|console>"
                                ++ "\n\nTo invoke this command, build a JSON object matching this schema and pass it as a single argument. Alternatively, use traditional CLI flags as shown in the usage line above."
                                ++ "\n\nEach property has an `x-cli-kind` indicating its CLI invocation form:\n- \"keyword\": --name <value>\n- \"flag\": --name (present or absent, no value)\n- \"keyword-list\": --name <value> (repeatable)"
                                ++ "\n\nPositional arguments are passed in order via the `$cli.positional` array (for this CLI it will always be empty)."
                    in
                    cfg
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string desc )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "format"
                                          , Encode.object
                                                [ ( "type", Encode.string "string" )
                                                , ( "enum"
                                                  , Encode.list Encode.string [ "json", "junit", "console" ]
                                                  )
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
            ]
        ]
