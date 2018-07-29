module Main exposing (main)

import Cli.OptionsParser as OptionsParser exposing (OptionsParser, with)
import Cli.ExitStatus
import Cli.Option
import Cli.Program
import Json.Decode exposing (..)
import Ports


type GitOptionsParser
    = Init
    | Clone String
    | Log LogOptions


type alias LogOptions =
    { maybeAuthorPattern : Maybe String
    , maybeMaxCount : Maybe Int
    , statisticsMode : Bool
    , maybeRevisionRange : Maybe String
    , restArgs : List String
    }


cli : Cli.Program.Program GitOptionsParser
cli =
    Cli.Program.program
        { programName = "git"
        , version = "1.2.3"
        }
        |> Cli.Program.add
            (OptionsParser.buildSubCommand "init" Init
                |> OptionsParser.withDoc "initialize a git repository"
            )
        |> Cli.Program.add
            (OptionsParser.buildSubCommand "clone" Clone
                |> with (Cli.Option.positionalArg "repository")
            )
        |> Cli.Program.add (OptionsParser.map Log logOptionsParser)


logOptionsParser : OptionsParser.TerminalOptionsParser LogOptions
logOptionsParser =
    OptionsParser.buildSubCommand "log" LogOptions
        |> with (Cli.Option.optionalKeywordArg "author")
        |> with
            (Cli.Option.optionalKeywordArg "max-count"
                |> Cli.Option.validateMapIfPresent String.toInt
            )
        |> with (Cli.Option.flag "stat")
        |> OptionsParser.endWith
            (Cli.Option.optionalPositionalArg "revision range")
        |> OptionsParser.finally
            (Cli.Option.restArgs "rest args")


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


type alias Flags =
    List String


init : Flags -> ( Model, Cmd Msg )
init argv =
    let
        matchResult : Cli.Program.RunResult GitOptionsParser
        matchResult =
            Cli.Program.run cli argv

        cmd =
            case matchResult of
                Cli.Program.SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.ExitStatus.Failure ->
                            Ports.printAndExitFailure message

                        Cli.ExitStatus.Success ->
                            Ports.printAndExitSuccess message

                Cli.Program.CustomMatch msg ->
                    (case msg of
                        Init ->
                            "Initializing test suite..."

                        Clone url ->
                            "Cloning `" ++ url ++ "`..."

                        Log options ->
                            [ "Logging..." |> Just
                            , options.maybeAuthorPattern |> Maybe.map (\authorPattern -> "authorPattern: " ++ authorPattern)
                            , options.maybeMaxCount |> Maybe.map (\maxCount -> "maxCount: " ++ toString maxCount)
                            , toString options.statisticsMode |> Just
                            , options.maybeRevisionRange |> Maybe.map (\revisionRange -> "revisionRange: " ++ toString revisionRange)
                            ]
                                |> List.filterMap identity
                                |> String.join "\n"
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
