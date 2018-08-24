module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (with)
import Cli.Program as Program
import Ports


type CliOptions
    = Init
    | RunTests RunTestsRecord


type alias RunTestsRecord =
    { maybeFuzz : Maybe Int
    , maybeSeed : Maybe Int
    , maybeCompilerPath : Maybe String
    , maybeDependencies : Maybe String
    , watch : Bool
    , reportFormat : ReportFormat
    , testFiles : List String
    }


type ReportFormat
    = Json
    | Junit
    | Console


program : Program.Config CliOptions
program =
    Program.config { version = "1.2.3" }
        |> Program.add
            (OptionsParser.buildSubCommand "init" Init
                |> OptionsParser.end
            )
        |> Program.add
            (OptionsParser.build RunTestsRecord
                |> with
                    (Option.optionalKeywordArg "fuzz"
                        |> Option.validateMapIfPresent (String.toInt >> maybeToResult)
                    )
                |> with
                    (Option.optionalKeywordArg "seed"
                        |> Option.validateMapIfPresent (String.toInt >> maybeToResult)
                    )
                |> with (Option.optionalKeywordArg "compiler")
                |> with (Option.optionalKeywordArg "add-dependencies")
                |> with (Option.flag "watch")
                |> with
                    (Option.optionalKeywordArg "report"
                        |> Option.withDefault "console"
                        |> Option.oneOf Console
                            [ ( "json", Json )
                            , ( "junit", Junit )
                            , ( "console", Console )
                            ]
                    )
                |> OptionsParser.withRestArgs (Option.restArgs "TESTFILES")
                |> OptionsParser.map RunTests
            )


init : Flags -> CliOptions -> Cmd Never
init flags msg =
    (case msg of
        Init ->
            "Initializing test suite..."

        RunTests options ->
            [ "Running the following test files: " ++ Debug.toString options.testFiles |> Just
            , "watch: " ++ Debug.toString options.watch |> Just
            , options.maybeFuzz |> Maybe.map (\fuzz -> "fuzz: " ++ Debug.toString fuzz)
            , options.maybeSeed |> Maybe.map (\seed -> "seed: " ++ String.fromInt seed)
            , options.reportFormat |> Debug.toString |> Just
            , options.maybeCompilerPath |> Maybe.map (\compilerPath -> "compiler: " ++ Debug.toString compilerPath)
            , options.maybeDependencies |> Maybe.map (\dependencies -> "dependencies: " ++ Debug.toString dependencies)
            ]
                |> List.filterMap identity
                |> String.join "\n"
    )
        |> Ports.print


type alias Flags =
    Program.FlagsIncludingArgv {}


maybeToResult : Maybe value -> Result String value
maybeToResult maybe =
    case maybe of
        Just value ->
            Ok value

        Nothing ->
            Err "Could not convert."


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = program
        }
