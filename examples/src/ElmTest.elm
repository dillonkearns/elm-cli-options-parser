module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser, with)
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports


type ElmTestOptionsParser
    = Init
    | RunTests RunTestsRecord


type alias RunTestsRecord =
    { maybeFuzz : Maybe Int
    , maybeSeed : Maybe Int
    , maybeCompilerPath : Maybe String
    , maybeDependencies : Maybe String
    , watch : Bool
    , report : Report
    , testFiles : List String
    }


program : Program.Program ElmTestOptionsParser
program =
    Program.program { version = "1.2.3" }
        |> Program.add
            (OptionsParser.buildSubCommand "init" Init
                |> OptionsParser.end
            )
        |> Program.add
            (OptionsParser.build RunTestsRecord
                |> with
                    (Option.optionalKeywordArg "fuzz"
                        |> Option.validateMapIfPresent String.toInt
                    )
                |> with
                    (Option.optionalKeywordArg "seed"
                        |> Option.validateMapIfPresent String.toInt
                    )
                |> with (Option.optionalKeywordArg "compiler")
                |> with (Option.optionalKeywordArg "add-dependencies")
                |> with (Option.flag "watch")
                |> with
                    (Option.optionalKeywordArg "report"
                        |> Option.withDefault "console"
                        |> Option.oneOf Console
                            [ "json" => Json
                            , "junit" => Junit
                            , "console" => Console
                            ]
                    )
                |> OptionsParser.restArgs (Option.restArgs "TESTFILES")
                |> OptionsParser.map RunTests
            )


type Report
    = Json
    | Junit
    | Console


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string



-- init : Flags -> ( Model, Cmd Msg )


init : ElmTestOptionsParser -> Cmd Never
init msg =
    (case msg of
        Init ->
            "Initializing test suite..."

        RunTests options ->
            [ "Running the following test files: " ++ toString options.testFiles |> Just
            , "watch: " ++ toString options.watch |> Just
            , options.maybeFuzz |> Maybe.map (\fuzz -> "fuzz: " ++ toString fuzz)
            , options.maybeSeed |> Maybe.map (\seed -> "seed: " ++ toString seed)
            , options.report |> toString |> Just
            , options.maybeCompilerPath |> Maybe.map (\compilerPath -> "compiler: " ++ toString compilerPath)
            , options.maybeDependencies |> Maybe.map (\dependencies -> "dependencies: " ++ toString dependencies)
            ]
                |> List.filterMap identity
                |> String.join "\n"
    )
        |> Ports.print


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


main : Program.ProgramNew Never
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , program = program
        }
