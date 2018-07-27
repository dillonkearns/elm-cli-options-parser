module Main exposing (main)

import Cli.Command as Command exposing (Command, with)
import Cli.ExitStatus
import Cli.Option as Option
import Cli.OptionsParser
import Json.Decode exposing (..)
import Ports


type ElmTestCommand
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


cli : Cli.OptionsParser.Program ElmTestCommand
cli =
    { programName = "elm-test"
    , commands = commands
    , version = "1.2.3"
    }


commands : List (Command ElmTestCommand)
commands =
    [ Command.buildSubCommand "init" Init
        |> Command.withoutRestArgs
    , Command.build RunTestsRecord
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
        |> Command.endWith (Option.restArgs "TESTFILES")
        |> Command.map RunTests
    ]


type Report
    = Json
    | Junit
    | Console


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


type alias Flags =
    List String


init : Flags -> ( Model, Cmd Msg )
init argv =
    let
        matchResult =
            Cli.OptionsParser.run cli argv

        toPrint =
            case matchResult of
                Cli.OptionsParser.SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.ExitStatus.Failure ->
                            Ports.printAndExitFailure message

                        Cli.ExitStatus.Success ->
                            Ports.printAndExitSuccess message

                Cli.OptionsParser.CustomMatch msg ->
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
    in
    ( (), toPrint )


type alias Model =
    ()


type alias Msg =
    ()


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
