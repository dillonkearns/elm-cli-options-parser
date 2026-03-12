module ExperienceTests exposing (all)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Expect
import Json.Encode as Encode
import Test exposing (..)
import TsJson.Decode as TsDecode


{-| A realistic CLI: a task management tool with subcommands.

    mytool add --title "Buy milk" --priority high
    mytool list --format json --limit 10
    mytool complete 42

-}
type CliOptions
    = Add AddOptions
    | ListTasks ListOptions
    | Complete CompleteOptions


type alias AddOptions =
    { title : String
    , priority : Priority
    }


type Priority
    = Low
    | Medium
    | High


type alias ListOptions =
    { format : Format
    , limit : Int
    , verbose : Bool
    }


type Format
    = Json
    | Table
    | Csv


type alias CompleteOptions =
    { taskId : String
    }


{-| The CLI config as a developer would write it.
-}
taskConfig : Program.Config CliOptions
taskConfig =
    Program.config
        |> Program.add
            (OptionsParser.buildSubCommand "add" AddOptions
                |> OptionsParser.with
                    (Option.requiredKeywordArg "title"
                        |> Option.withDescription "The task title"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "priority"
                        |> Option.oneOf
                            [ ( "low", Low )
                            , ( "medium", Medium )
                            , ( "high", High )
                            ]
                        |> Option.withDescription "Task priority level"
                    )
                |> OptionsParser.map Add
            )
        |> Program.add
            (OptionsParser.buildSubCommand "list" ListOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "format"
                        |> Option.withDefault "table"
                        |> Option.oneOf
                            [ ( "json", Json )
                            , ( "table", Table )
                            , ( "csv", Csv )
                            ]
                        |> Option.withDescription "Output format"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "limit"
                        |> Option.validateMap
                            (\s ->
                                case String.toInt s of
                                    Just n ->
                                        if n > 0 then
                                            Ok n

                                        else
                                            Err "limit must be a positive integer"

                                    Nothing ->
                                        Err ("expected an integer but got: " ++ s)
                            )
                        |> Option.withDescription "Maximum number of tasks to show"
                    )
                |> OptionsParser.with
                    (Option.flag "verbose"
                        |> Option.withDescription "Show full task details"
                    )
                |> OptionsParser.map ListTasks
            )
        |> Program.add
            (OptionsParser.buildSubCommand "complete" CompleteOptions
                |> OptionsParser.with
                    (Option.requiredPositionalArg "task-id"
                        |> Option.withDescription "The ID of the task to mark complete"
                    )
                |> OptionsParser.map Complete
            )


{-| A simpler CLI that shows withTypedJson.

    deploy --config '{"host":"prod.example.com","port":443,"ssl":true}'

-}
type alias DeployOptions =
    { config : DeployConfig
    , dryRun : Bool
    }


type alias DeployConfig =
    { host : String
    , port_ : Int
    , ssl : Bool
    }


deployConfigDecoder : TsDecode.Decoder DeployConfig
deployConfigDecoder =
    TsDecode.map3 DeployConfig
        (TsDecode.field "host" TsDecode.string)
        (TsDecode.field "port" TsDecode.int)
        (TsDecode.field "ssl" TsDecode.bool)


deployConfig : Program.Config DeployOptions
deployConfig =
    Program.config
        |> Program.add
            (OptionsParser.build DeployOptions
                |> OptionsParser.with
                    (Option.requiredKeywordArg "config"
                        |> Option.withTypedJson deployConfigDecoder
                        |> Option.withDescription "Deployment configuration"
                    )
                |> OptionsParser.with
                    (Option.flag "dry-run"
                        |> Option.withDescription "Preview changes without deploying"
                    )
            )


all : Test
all =
    describe "Developer & User Experience"
        [ describe "1. JSON Schema output (what LLMs see)"
            [ test "task manager schema" <|
                \() ->
                    taskConfig
                        |> Program.toJsonSchema
                        |> Encode.encode 2
                        |> Expect.equal """{
  "$cli": "elm-cli-options-parser",
  "anyOf": [
    {
      "type": "object",
      "properties": {
        "subcommand": {
          "type": "string",
          "const": "add"
        },
        "title": {
          "type": "string",
          "description": "The task title"
        },
        "priority": {
          "anyOf": [
            {
              "const": "low"
            },
            {
              "const": "medium"
            },
            {
              "const": "high"
            }
          ],
          "description": "Task priority level"
        }
      },
      "required": [
        "subcommand",
        "title",
        "priority"
      ]
    },
    {
      "type": "object",
      "properties": {
        "subcommand": {
          "type": "string",
          "const": "list"
        },
        "format": {
          "anyOf": [
            {
              "const": "json"
            },
            {
              "const": "table"
            },
            {
              "const": "csv"
            }
          ],
          "description": "Output format"
        },
        "limit": {
          "type": "string",
          "description": "Maximum number of tasks to show"
        },
        "verbose": {
          "type": "boolean",
          "description": "Show full task details"
        }
      },
      "required": [
        "subcommand",
        "limit"
      ]
    },
    {
      "type": "object",
      "properties": {
        "subcommand": {
          "type": "string",
          "const": "complete"
        },
        "task-id": {
          "type": "string",
          "description": "The ID of the task to mark complete"
        }
      },
      "required": [
        "subcommand",
        "task-id"
      ]
    }
  ]
}"""
            , test "deploy tool schema (with typed JSON)" <|
                \() ->
                    deployConfig
                        |> Program.toJsonSchema
                        |> Encode.encode 2
                        |> Expect.equal """{
  "$cli": "elm-cli-options-parser",
  "type": "object",
  "properties": {
    "config": {
      "type": "object",
      "properties": {
        "host": {
          "type": "string"
        },
        "port": {
          "type": "integer"
        },
        "ssl": {
          "type": "boolean"
        }
      },
      "required": [
        "host",
        "port",
        "ssl"
      ],
      "description": "Deployment configuration"
    },
    "dry-run": {
      "type": "boolean",
      "description": "Preview changes without deploying"
    }
  },
  "required": [
    "config"
  ]
}"""
            ]
        , describe "2. Help text (what users see with --help)"
            [ test "task manager help" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "--help" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Success
                                """Usage: mytool add --title <TITLE> --priority <low|medium|high>

Options:
  --title <TITLE>                The task title
  --priority <low|medium|high>   Task priority level

Usage: mytool list [--format <json|table|csv>] --limit <LIMIT> [--verbose]

Options:
  --format <json|table|csv>   Output format
  --limit <LIMIT>             Maximum number of tasks to show
  --verbose                   Show full task details

Usage: mytool complete <task-id>

Options:
  <task-id>   The ID of the task to mark complete"""
                            )
            , test "task manager subcommand help" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "list", "--help" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Success
                                """Usage: mytool list [--format <json|table|csv>] --limit <LIMIT> [--verbose]

Options:
  --format <json|table|csv>   Output format
  --limit <LIMIT>             Maximum number of tasks to show
  --verbose                   Show full task details"""
                            )
            , test "deploy tool help" <|
                \() ->
                    Program.run deployConfig
                        [ "node", "deploy", "--help" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Success
                                """Usage: deploy --config <JSON> [--dry-run]

Options:
  --config <JSON>   Deployment configuration
  --dry-run         Preview changes without deploying"""
                            )
            ]
        , describe "3a. CLI mode - correct usage"
            [ test "add task via CLI" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "add", "--title", "Buy milk", "--priority", "high" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (Add { title = "Buy milk", priority = High }))
            , test "list tasks via CLI" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "list", "--format", "json", "--limit", "10", "--verbose" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (ListTasks { format = Json, limit = 10, verbose = True }))
            , test "complete task via CLI" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "complete", "42" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (Complete { taskId = "42" }))
            , test "deploy via CLI with JSON string arg" <|
                \() ->
                    Program.run deployConfig
                        [ "node", "deploy", "--config", "{\"host\":\"prod.example.com\",\"port\":443,\"ssl\":true}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch { config = { host = "prod.example.com", port_ = 443, ssl = True }, dryRun = False })
            ]
        , describe "3b. JSON input mode - correct usage"
            [ test "add task via JSON" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":\"elm-cli-options-parser\",\"subcommand\":\"add\",\"title\":\"Buy milk\",\"priority\":\"high\"}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (Add { title = "Buy milk", priority = High }))
            , test "list tasks via JSON" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":\"elm-cli-options-parser\",\"subcommand\":\"list\",\"format\":\"json\",\"limit\":\"10\",\"verbose\":true}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (ListTasks { format = Json, limit = 10, verbose = True }))
            , test "complete task via JSON" <|
                \() ->
                    -- Direct JSON decoding: positional args are just named fields in JSON
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":\"elm-cli-options-parser\",\"subcommand\":\"complete\",\"task-id\":\"42\"}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (Complete { taskId = "42" }))
            , test "deploy via JSON - typed JSON arg gets nested object" <|
                \() ->
                    -- With direct JSON decoding, the nested object is decoded natively
                    -- No round-trip through string serialization
                    Program.run deployConfig
                        [ "node", "deploy", "{\"$cli\":\"elm-cli-options-parser\",\"config\":{\"host\":\"prod.example.com\",\"port\":443,\"ssl\":true}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch { config = { host = "prod.example.com", port_ = 443, ssl = True }, dryRun = False })
            ]
        , describe "4a. CLI mode - error messages"
            [ test "missing required option" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "add", "--priority", "high" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Missing required option: --title

mytool add --title <TITLE> --priority <low|medium|high>
mytool list [--format <json|table|csv>] --limit <LIMIT> [--verbose]
mytool complete <task-id>"""
                            )
            , test "invalid oneOf value" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "add", "--title", "Buy milk", "--priority", "urgent" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid `--priority` option.
Must be one of [low, medium, high]"""
                            )
            , test "invalid integer (non-numeric string)" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "list", "--format", "json", "--limit", "abc" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid `--limit` option.
expected an integer but got: abc"""
                            )
            , test "invalid integer (negative)" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "list", "--format", "json", "--limit", "-5" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid `--limit` option.
limit must be a positive integer"""
                            )
            , test "unknown subcommand" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "delete" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Unknown command: `delete`

Available commands: add, list, complete

Run with --help for usage information."""
                            )
            , test "unknown option (typo)" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "add", "--title", "Buy milk", "--pririty", "high" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """The `--pririty` flag was not found. Maybe it was one of these typos?

`--pririty` <> `--priority`"""
                            )
            , test "invalid typed JSON in CLI mode" <|
                \() ->
                    Program.run deployConfig
                        [ "node", "deploy", "--config", "{\"host\":\"prod.example.com\",\"port\":\"not-a-number\"}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid `--config` option.
Problem with the value at json.port:

    "not-a-number"

Expecting an INT"""
                            )
            ]
        , describe "4b. JSON input mode - error messages"
            [ test "missing required field in JSON" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":\"elm-cli-options-parser\",\"subcommand\":\"add\",\"priority\":\"high\"}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                "Missing required field: \"title\""
                            )
            , test "invalid oneOf value in JSON" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":\"elm-cli-options-parser\",\"subcommand\":\"add\",\"title\":\"Buy milk\",\"priority\":\"urgent\"}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid "priority" field.
Must be one of [low, medium, high]"""
                            )
            , test "wrong type for limit in JSON (number instead of string)" <|
                \() ->
                    -- With direct JSON decoding, JSON number 10 for a string field is a type error
                    -- The schema says "type": "string", so LLMs should send "10" not 10
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":\"elm-cli-options-parser\",\"subcommand\":\"list\",\"format\":\"json\",\"limit\":10,\"verbose\":true}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid "limit" field.
Problem with the value at json.limit:

    10

Expecting a STRING"""
                            )
            , test "invalid typed JSON in JSON mode" <|
                \() ->
                    Program.run deployConfig
                        [ "node", "deploy", "{\"$cli\":\"elm-cli-options-parser\",\"config\":{\"host\":\"prod.example.com\",\"port\":\"not-a-number\"}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid "config" field.
Problem with the value at json.config.port:

    "not-a-number"

Expecting an INT"""
                            )
            ]
        , describe "5. String vs int type difference"
            [ test "limit as string '10' works (CLI)" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "list", "--format", "table", "--limit", "10" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (ListTasks { format = Table, limit = 10, verbose = False }))
            , test "limit as number 10 in JSON fails (no silent coercion)" <|
                \() ->
                    -- With direct JSON decoding, number 10 for a string field is a type error.
                    -- The schema says "type": "string" for limit. LLMs should send "10" not 10.
                    -- No more silent number-to-string coercion.
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":\"elm-cli-options-parser\",\"subcommand\":\"list\",\"format\":\"table\",\"limit\":10}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid "limit" field.
Problem with the value at json.limit:

    10

Expecting a STRING"""
                            )
            , test "port in typed JSON is a real integer (no string coercion)" <|
                \() ->
                    -- With withTypedJson, the decoder expects an actual integer
                    -- Passing a string "443" for port would FAIL
                    Program.run deployConfig
                        [ "node", "deploy", "--config", "{\"host\":\"prod.example.com\",\"port\":443,\"ssl\":true}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch { config = { host = "prod.example.com", port_ = 443, ssl = True }, dryRun = False })
            , test "port as string in typed JSON fails with type error" <|
                \() ->
                    -- This SHOULD fail because the TsJson decoder expects int, not string
                    Program.run deployConfig
                        [ "node", "deploy", "--config", "{\"host\":\"prod.example.com\",\"port\":\"443\",\"ssl\":true}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                """Validation errors:

Invalid `--config` option.
Problem with the value at json.port:

    "443"

Expecting an INT"""
                            )
            ]
        ]
