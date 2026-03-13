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
                                , ( "anyOf"
                                  , Encode.list identity
                                        [ Encode.object [ ( "const", Encode.string "json" ) ]
                                        , Encode.object [ ( "const", Encode.string "junit" ) ]
                                        , Encode.object [ ( "const", Encode.string "console" ) ]
                                        ]
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
                    in
                    cfg
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 0
                        |> Expect.equal
                            (Encode.object
                                [ ( "description", Encode.string "test --format <json|junit|console>" )
                                , ( "type", Encode.string "object" )
                                , ( "properties"
                                  , Encode.object
                                        [ ( "$cli"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
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
                                                                                , ( "anyOf"
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
