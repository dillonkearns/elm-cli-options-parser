module ExperienceTests exposing (all)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Expect
import Json.Encode as Encode
import Test exposing (..)


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


all : Test
all =
    describe "Developer & User Experience"
        [ describe "1. JSON Schema output (what LLMs see)"
            [ test "task manager schema" <|
                \() ->
                    taskConfig
                        |> Program.toJsonSchema "test"
                        |> Encode.encode 2
                        |> Expect.equal """{
  "anyOf": [
    {
      "description": "Usage: test add --title <TITLE> --priority <low|medium|high>\\n\\nOptions:\\n  --title <TITLE>                The task title\\n  --priority <low|medium|high>   Task priority level",
      "type": "object",
      "properties": {
        "$cli": {
          "type": "object",
          "description": "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable.",
          "properties": {
            "subcommand": {
              "type": "string",
              "const": "add"
            },
            "keywordValues": {
              "type": "object",
              "description": "Keyword arguments with values (e.g., --name <value>)",
              "properties": {
                "title": {
                  "type": "string",
                  "description": "The task title"
                },
                "priority": {
                  "type": "string",
                  "enum": [
                    "low",
                    "medium",
                    "high"
                  ],
                  "description": "Task priority level"
                }
              },
              "required": [
                "title",
                "priority"
              ]
            }
          },
          "required": [
            "subcommand",
            "keywordValues"
          ]
        }
      },
      "required": [
        "$cli"
      ]
    },
    {
      "description": "Usage: test list [--format <json|table|csv>] --limit <LIMIT> [--verbose]\\n\\nOptions:\\n  --format <json|table|csv>   Output format\\n  --limit <LIMIT>             Maximum number of tasks to show\\n  --verbose                   Show full task details",
      "type": "object",
      "properties": {
        "$cli": {
          "type": "object",
          "description": "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable.",
          "properties": {
            "subcommand": {
              "type": "string",
              "const": "list"
            },
            "keywordValues": {
              "type": "object",
              "description": "Keyword arguments with values (e.g., --name <value>)",
              "properties": {
                "format": {
                  "type": "string",
                  "enum": [
                    "json",
                    "table",
                    "csv"
                  ],
                  "description": "Output format"
                },
                "limit": {
                  "type": "string",
                  "description": "Maximum number of tasks to show"
                }
              },
              "required": [
                "limit"
              ]
            },
            "flags": {
              "type": "object",
              "description": "Boolean flags, passed as --flag (e.g., --verbose)",
              "properties": {
                "verbose": {
                  "type": "boolean",
                  "description": "Show full task details"
                }
              }
            }
          },
          "required": [
            "subcommand",
            "keywordValues"
          ]
        }
      },
      "required": [
        "$cli"
      ]
    },
    {
      "description": "Usage: test complete <task-id>\\n\\nOptions:\\n  <task-id>   The ID of the task to mark complete",
      "type": "object",
      "properties": {
        "$cli": {
          "type": "object",
          "description": "CLI input: contains keywordValues, flags, positional args, and subcommand as applicable.",
          "properties": {
            "subcommand": {
              "type": "string",
              "const": "complete"
            },
            "positional": {
              "type": "array",
              "description": "Positional arguments, passed in order (e.g., mytool <source> <dest>)",
              "prefixItems": [
                {
                  "type": "string",
                  "description": "The ID of the task to mark complete"
                }
              ],
              "items": false,
              "minItems": 1
            }
          },
          "required": [
            "subcommand"
          ]
        }
      },
      "required": [
        "$cli"
      ]
    }
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
            ]
        , describe "3b. JSON input mode - correct usage"
            [ test "add task via JSON" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":{\"subcommand\":\"add\",\"keywordValues\":{\"title\":\"Buy milk\",\"priority\":\"high\"}}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (Add { title = "Buy milk", priority = High }))
            , test "list tasks via JSON" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":{\"subcommand\":\"list\",\"flags\":{\"verbose\":true},\"keywordValues\":{\"format\":\"json\",\"limit\":\"10\"}}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (ListTasks { format = Json, limit = 10, verbose = True }))
            , test "complete task via JSON" <|
                \() ->
                    -- Direct JSON decoding: positional args come from $cli.positional
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":{\"subcommand\":\"complete\",\"positional\":[\"42\"]}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal (Program.CustomMatch (Complete { taskId = "42" }))
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
            ]
        , describe "4b. JSON input mode - error messages"
            [ test "missing required field in JSON" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":{\"subcommand\":\"add\",\"keywordValues\":{\"priority\":\"high\"}}}" ]
                        "1.0.0"
                        Program.WithoutColor
                        |> Expect.equal
                            (Program.SystemMessage Program.Failure
                                "Missing required field: \"title\""
                            )
            , test "invalid oneOf value in JSON" <|
                \() ->
                    Program.run taskConfig
                        [ "node", "mytool", "{\"$cli\":{\"subcommand\":\"add\",\"keywordValues\":{\"title\":\"Buy milk\",\"priority\":\"urgent\"}}}" ]
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
                        [ "node", "mytool", "{\"$cli\":{\"subcommand\":\"list\",\"keywordValues\":{\"format\":\"json\",\"limit\":10}}}" ]
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
                        [ "node", "mytool", "{\"$cli\":{\"subcommand\":\"list\",\"keywordValues\":{\"format\":\"table\",\"limit\":10}}}" ]
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
            ]
        ]
