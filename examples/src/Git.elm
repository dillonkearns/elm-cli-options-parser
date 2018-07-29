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
    | Log LogOptions


type alias LogOptions =
    { maybeAuthorPattern : Maybe String
    , maybeMaxCount : Maybe Int
    , statisticsMode : Bool
    , maybeRevisionRange : Maybe String
    , restArgs : List String
    }


cli : Cli.OptionsParser.Program GitCommand
cli =
    Cli.OptionsParser.empty
        { programName = "git"
        , version = "1.2.3"
        }
        |> Cli.OptionsParser.add
            (Command.buildSubCommand "init" Init
                |> Command.withDoc "initialize a git repository"
            )
        |> Cli.OptionsParser.add
            (Command.buildSubCommand "clone" Clone
                |> with (Cli.Option.positionalArg "repository")
            )
        |> Cli.OptionsParser.add (Command.map Log logCommand)


logCommand : Command.TerminalCommand LogOptions
logCommand =
    Command.buildSubCommand "log" LogOptions
        |> with (Cli.Option.optionalKeywordArg "author")
        |> with
            (Cli.Option.optionalKeywordArg "max-count"
                |> Cli.Option.validateMapIfPresent String.toInt
            )
        |> with (Cli.Option.flag "stat")
        |> Command.endWith
            (Cli.Option.optionalPositionalArg "revision range")
        |> Command.finally
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
