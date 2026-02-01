module Snapshots exposing (run)

{-| Snapshot tests for ANSI color output.

These tests capture the exact output (including ANSI escape codes) for various
CLI scenarios. The snapshots can be viewed in VS Code or other terminals that
render ANSI codes to verify colors are applied correctly.

Each scenario is tested with both WithColor and WithoutColor modes to:
1. Verify the colored output looks correct
2. Compare against plain text to see exactly where colors are applied

-}

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Pages.Script exposing (Script)
import Snapshot
import Snapshot.Printer as Printer



-- SAMPLE CONFIGS


{-| Git-like config with subcommands for testing various error scenarios.
-}
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


{-| Simple config with required keyword arg.
-}
greetConfig : Program.Config String
greetConfig =
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.with (Option.requiredKeywordArg "name")
            )


{-| Config with just a flag for testing typo suggestions.
-}
verboseConfig : Program.Config Bool
verboseConfig =
    Program.config
        |> Program.add
            (OptionsParser.build identity
                |> OptionsParser.with (Option.flag "verbose")
            )


{-| Config with option descriptions for help text testing.
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


{-| Config with multiple option types for comprehensive help testing.
-}
comprehensiveConfig : Program.Config { output : Maybe String, verbose : Bool, force : Bool, input : String }
comprehensiveConfig =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\output verbose force input ->
                    { output = output, verbose = verbose, force = force, input = input }
                )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "output"
                        |> Option.withDescription "Output file path"
                    )
                |> OptionsParser.with
                    (Option.flag "verbose"
                        |> Option.withDescription "Enable verbose output"
                    )
                |> OptionsParser.with
                    (Option.flag "force"
                        |> Option.withDescription "Force overwrite existing files"
                    )
                |> OptionsParser.with
                    (Option.requiredPositionalArg "input"
                        |> Option.withDescription "Input file to process"
                    )
            )



-- HELPERS


{-| Printer that outputs .ansi files for proper color rendering in VS Code.
-}
ansiPrinter : Printer.Printer String
ansiPrinter =
    Printer.string
        |> Printer.withExtension "ansi"


{-| Run CLI and extract the output message (whether success or failure).
Includes the command line at the top for easy inspection.
-}
runAndGetOutput : Program.Config msg -> List String -> String -> Program.ColorMode -> String
runAndGetOutput config args version colorMode =
    let
        commandLine =
            "$ myprog " ++ String.join " " args

        output =
            case Program.run config ("/usr/bin/node" :: "/path/to/myprog" :: args) version colorMode of
                Program.SystemMessage _ message ->
                    message

                Program.CustomMatch _ ->
                    "<matched successfully - no output>"
    in
    commandLine ++ "\n\n" ++ output



-- TESTS


run : Script
run =
    Snapshot.run "Snapshots"
        [ Snapshot.describe "Error Messages"
            [ Snapshot.describe "Missing Subcommand"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput gitConfig [] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput gitConfig [] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Unknown Subcommand"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "status" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "status" ] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Missing Required Positional Arg"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "clone" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "clone" ] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Missing Required Keyword Arg"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput greetConfig [] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput greetConfig [] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Typo Suggestion"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput verboseConfig [ "--verboze" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput verboseConfig [ "--verboze" ] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Too Many Arguments"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "init", "extra", "args" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "init", "extra", "args" ] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Unknown Flag With Typo"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "--unknown-flag" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "--unknown-flag" ] "1.0.0" Program.WithoutColor
                ]
            ]
        , Snapshot.describe "Help Text"
            [ Snapshot.describe "Basic Help"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "--help" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "--help" ] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Help With Descriptions"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput configWithDescriptions [ "--help" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput configWithDescriptions [ "--help" ] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Subcommand Specific Help"
                [ Snapshot.custom ansiPrinter "clone-with-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "clone", "--help" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "clone-without-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "clone", "--help" ] "1.0.0" Program.WithoutColor
                , Snapshot.custom ansiPrinter "log-with-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "log", "--help" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "log-without-color" <|
                    \() ->
                        runAndGetOutput gitConfig [ "log", "--help" ] "1.0.0" Program.WithoutColor
                ]
            , Snapshot.describe "Comprehensive Help"
                [ Snapshot.custom ansiPrinter "with-color" <|
                    \() ->
                        runAndGetOutput comprehensiveConfig [ "--help" ] "1.0.0" Program.WithColor
                , Snapshot.custom ansiPrinter "without-color" <|
                    \() ->
                        runAndGetOutput comprehensiveConfig [ "--help" ] "1.0.0" Program.WithoutColor
                ]
            ]
        ]
