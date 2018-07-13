module Main exposing (main)

import Cli.Command as Command exposing (Command, with)
import Cli.OptionsParser
import Cli.Spec as Spec
import Json.Decode exposing (..)
import Ports


type ElmTestCommand
    = Init
    | RunTests RunTestsRecord
    | PrintVersion


type alias RunTestsRecord =
    { maybeFuzz : Maybe Int
    , maybeSeed : Maybe Int
    , maybeCompilerPath : Maybe String
    , maybeDependencies : Maybe String
    , watch : Bool
    , report : Report
    , testFiles : List String
    }


cli : List (Command ElmTestCommand)
cli =
    [ Command.subCommand "init" Init
        |> Command.withoutRestArgs
    , Command.build RunTestsRecord
        |> with
            (Spec.optionalKeywordArg "fuzz"
                |> Spec.validateMapMaybe String.toInt
            )
        |> with
            (Spec.optionalKeywordArg "seed"
                |> Spec.validateMapMaybe String.toInt
            )
        |> with (Spec.optionalKeywordArg "compiler")
        |> with (Spec.optionalKeywordArg "add-dependencies")
        |> with (Spec.flag "watch")
        |> with
            (Spec.optionalKeywordArg "report"
                |> Spec.withDefault "console"
                |> Spec.oneOf Console
                    [ Spec.MutuallyExclusiveValue "json" Json
                    , Spec.MutuallyExclusiveValue "junit" Junit
                    , Spec.MutuallyExclusiveValue "console" Console
                    ]
            )
        |> Command.withRestArgs "TESTFILES"
        |> Command.map RunTests
    , Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.withoutRestArgs
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
init flags =
    let
        matchResult =
            Cli.OptionsParser.run "elm-test" cli flags

        toPrint =
            case matchResult of
                Cli.OptionsParser.SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.OptionsParser.Failure ->
                            Ports.printAndExitFailure message

                        Cli.OptionsParser.Success ->
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

                        PrintVersion ->
                            "You are on version 3.1.4"
                    )
                        |> Ports.print
    in
    ( (), toPrint )


type alias Model =
    ()


type alias Msg =
    ()


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
