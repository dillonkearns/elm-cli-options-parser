module Main exposing (main)

import Cli.Command as Command exposing (Command, with)
import Cli.ExitStatus
import Cli.Option
import Cli.OptionsParser
import Json.Decode exposing (..)
import Ports


type GitCommand
    = Init
    | Clone String


type alias RunTestsRecord =
    { maybeFuzz : Maybe Int
    , maybeSeed : Maybe Int
    , maybeCompilerPath : Maybe String
    , maybeDependencies : Maybe String
    , watch : Bool
    , report : Report
    , testFiles : List String
    }


cli : Cli.OptionsParser.Program GitCommand
cli =
    { programName = "git"
    , commands = commands
    , version = "1.2.3"
    }


commands : List (Command GitCommand)
commands =
    [ Command.buildSubCommand "init" Init
        |> Command.withoutRestArgs
        |> Command.withDoc "initialize a git repository"
    , Command.buildSubCommand "clone" Clone
        |> with (Cli.Option.positionalArg "repository")
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
init argv =
    let
        matchResult : Cli.OptionsParser.RunResult GitCommand
        matchResult =
            Cli.OptionsParser.run cli argv

        cmd =
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

                        Clone url ->
                            "Cloning `" ++ url ++ "`..."
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
