module ErrorMessageFormattingTests exposing (all)

{-| Approval-style tests for error messages.

These tests assert on the exact error message output to ensure
users see clear, helpful messages.
-}

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Expect exposing (Expectation)
import Test exposing (..)



-- TEST HELPERS


{-| Run a CLI config with the given args and return the output.
-}
runCli : Program.Config msg -> List String -> Program.RunResult msg
runCli config args =
    Program.run config ("/usr/bin/node" :: "/path/to/myprog" :: args) "1.0.0"


{-| Assert that the CLI run produced an error with the exact message.
-}
expectError : String -> Program.RunResult msg -> Expectation
expectError expectedMessage result =
    case result of
        Program.SystemMessage Program.Failure actualMessage ->
            actualMessage |> Expect.equal expectedMessage

        Program.SystemMessage Program.Success message ->
            Expect.fail ("Expected error but got success:\n\n" ++ message)

        Program.CustomMatch _ ->
            Expect.fail "Expected error but got successful match"


{-| Assert that the CLI run succeeded with the given value.
-}
expectMatch : msg -> Program.RunResult msg -> Expectation
expectMatch expectedValue result =
    case result of
        Program.CustomMatch actualValue ->
            actualValue |> Expect.equal expectedValue

        Program.SystemMessage status message ->
            Expect.fail ("Expected match but got system message:\n\n" ++ message)


{-| Assert that the CLI run produced a success message (help/version).
-}
expectSuccess : String -> Program.RunResult msg -> Expectation
expectSuccess expectedMessage result =
    case result of
        Program.SystemMessage Program.Success actualMessage ->
            actualMessage |> Expect.equal expectedMessage

        Program.SystemMessage Program.Failure message ->
            Expect.fail ("Expected success but got error:\n\n" ++ message)

        Program.CustomMatch _ ->
            Expect.fail "Expected success message but got custom match"



-- SAMPLE CONFIGS


gitConfig : Program.Config String
gitConfig =
    Program.config
        |> Program.add
            (OptionsParser.buildSubCommand "init" "init")
        |> Program.add
            (OptionsParser.buildSubCommand "clone" (\_ -> "clone")
                |> OptionsParser.with (Option.requiredPositionalArg "repository")
            )
        |> Program.add
            (OptionsParser.buildSubCommand "log" (\_ _ -> "log")
                |> OptionsParser.with (Option.optionalKeywordArg "author")
                |> OptionsParser.with (Option.flag "oneline")
            )


greetConfig : Program.Config String
greetConfig =
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.with (Option.requiredKeywordArg "name")
            )


verboseConfig : Program.Config Bool
verboseConfig =
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.with (Option.flag "verbose")
            )


{-| Config that demonstrates withMissingMessage for custom error messages.
-}
gitCloneWithCustomMessage : Program.Config String
gitCloneWithCustomMessage =
    Program.config
        |> Program.add
            (OptionsParser.buildSubCommand "clone" identity
                |> OptionsParser.with
                    (Option.requiredPositionalArg "repository"
                        |> Option.withMissingMessage "You must specify a repository to clone."
                    )
            )


{-| Config with custom missing message for keyword arg.
-}
configWithCustomKeywordMessage : Program.Config String
configWithCustomKeywordMessage =
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.with
                    (Option.requiredKeywordArg "token"
                        |> Option.withMissingMessage "Authentication token is required. Get one from https://example.com/settings/tokens"
                    )
            )


{-| Config with descriptions for help text.
-}
configWithDescriptions : Program.Config ( String, Maybe String )
configWithDescriptions =
    Program.config
        |> Program.add
            (OptionsParser.build Tuple.pair
                |> OptionsParser.with
                    (Option.requiredKeywordArg "name"
                        |> Option.withDescription "Your name for the greeting"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "greeting"
                        |> Option.withDescription "Custom greeting message"
                    )
            )



-- TESTS


all : Test
all =
    describe "Error messages"
        [ describe "missing subcommand"
            [ test "shows available commands" <|
                \() ->
                    runCli gitConfig []
                        |> expectError
                            """Missing command.

Available commands: init, clone, log

Run with --help for usage information."""
            ]
        , describe "unknown subcommand"
            [ test "shows what was provided and lists available commands" <|
                \() ->
                    runCli gitConfig [ "status" ]
                        |> expectError
                            """Unknown command: `status`

Available commands: init, clone, log

Run with --help for usage information."""
            ]
        , describe "missing required positional arg"
            [ test "names the missing argument and shows usage" <|
                \() ->
                    runCli gitConfig [ "clone" ]
                        |> expectError
                            """Missing required argument: <repository>

myprog init
myprog clone <repository>
myprog log [--author <author>] [--oneline]"""
            ]
        , describe "missing required keyword arg"
            [ test "names the missing option and shows usage" <|
                \() ->
                    runCli greetConfig []
                        |> expectError
                            """Missing required option: --name

myprog --name <name>"""
            ]
        , describe "unknown flag"
            [ test "suggests similar flags" <|
                \() ->
                    runCli verboseConfig [ "--verboze" ]
                        |> expectError
                            """The `--verboze` flag was not found. Maybe it was one of these typos?

`--verboze` <> `--verbose`"""
            ]
        , describe "too many arguments"
            [ test "reports extra arguments and shows usage" <|
                \() ->
                    runCli gitConfig [ "init", "extra-arg" ]
                        |> expectError
                            """Too many arguments provided.

myprog init
myprog clone <repository>
myprog log [--author <author>] [--oneline]"""
            ]
        , describe "successful parsing"
            [ test "subcommand matches" <|
                \() ->
                    runCli gitConfig [ "init" ]
                        |> expectMatch "init"
            , test "subcommand with args matches" <|
                \() ->
                    runCli gitConfig [ "clone", "https://github.com/foo/bar" ]
                        |> expectMatch "clone"
            , test "required keyword arg parses" <|
                \() ->
                    runCli greetConfig [ "--name", "World" ]
                        |> expectMatch "World"
            ]
        , describe "help and version"
            [ test "--help shows usage" <|
                \() ->
                    runCli gitConfig [ "--help" ]
                        |> expectSuccess
                            """Usage: myprog init

Usage: myprog clone <repository>

Usage: myprog log [--author <author>] [--oneline]"""
            , test "--version shows version" <|
                \() ->
                    Program.run gitConfig [ "node", "myprog", "--version" ] "2.5.0"
                        |> expectSuccess "2.5.0"
            ]
        , describe "custom missing messages (withMissingMessage)"
            [ test "positional arg with custom message" <|
                \() ->
                    runCli gitCloneWithCustomMessage [ "clone" ]
                        |> expectError
                            """You must specify a repository to clone.

myprog clone <repository>"""
            , test "keyword arg with custom message" <|
                \() ->
                    runCli configWithCustomKeywordMessage []
                        |> expectError
                            """Authentication token is required. Get one from https://example.com/settings/tokens

myprog --token <token>"""
            , test "custom message with successful parse" <|
                \() ->
                    runCli gitCloneWithCustomMessage [ "clone", "https://github.com/foo/bar" ]
                        |> expectMatch "https://github.com/foo/bar"
            ]
        , describe "option descriptions (withDescription)"
            [ test "config with descriptions parses correctly" <|
                \() ->
                    runCli configWithDescriptions [ "--name", "Alice", "--greeting", "Hello" ]
                        |> expectMatch ( "Alice", Just "Hello" )
            , test "help text shows descriptions in Options section" <|
                \() ->
                    runCli configWithDescriptions [ "--help" ]
                        |> expectSuccess
                            """Usage: myprog --name <name> [--greeting <greeting>]

Options:
  --name <name>           Your name for the greeting
  --greeting <greeting>   Custom greeting message"""
            ]
        , describe "subcommand-specific help"
            [ test "subcommand --help shows only that subcommand's usage" <|
                \() ->
                    runCli gitConfig [ "clone", "--help" ]
                        |> expectSuccess "Usage: myprog clone <repository>"
            , test "subcommand with options --help shows usage" <|
                \() ->
                    runCli gitConfig [ "log", "--help" ]
                        |> expectSuccess "Usage: myprog log [--author <author>] [--oneline]"
            ]
        ]
