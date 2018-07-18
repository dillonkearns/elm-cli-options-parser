module Main exposing (main)

import Cli.Command as Command exposing (Command, with)
import Cli.OptionsParser
import Json.Decode exposing (..)
import Ports


type ElmTestCommand
    = Init
    | Clone
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
    [ Command.buildSubCommand "init" Init
        |> Command.withoutRestArgs
        |> Command.withDoc "initialize a git repository"
    , Command.buildSubCommand "clone" Clone
        |> Command.withoutRestArgs
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
            Cli.OptionsParser.run "git" cli flags

        cmd =
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

                        Clone ->
                            "Cloning..."

                        PrintVersion ->
                            "You are on version 3.1.4"
                    )
                        |> Ports.print
    in
    ( (), cmd )


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
