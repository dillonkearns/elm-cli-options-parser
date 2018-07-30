module Main exposing (main)

import Cli.ExitStatus
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser, with)
import Cli.Program as Program
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


cli : Program.Program GitOptionsParser
cli =
    { programName = "git"
    , version = "1.2.3"
    }
        |> Program.program
        |> Program.add
            (OptionsParser.buildSubCommand "init" Init
                |> OptionsParser.withDoc "initialize a git repository"
            )
        |> Program.add
            (OptionsParser.buildSubCommand "clone" Clone
                |> with (Option.positionalArg "repository")
            )
        |> Program.add (OptionsParser.map Log logOptionsParser)


logOptionsParser : OptionsParser.TerminalOptionsParser LogOptions
logOptionsParser =
    OptionsParser.buildSubCommand "log" LogOptions
        |> with (Option.optionalKeywordArg "author")
        |> with
            (Option.optionalKeywordArg "max-count"
                |> Option.validateMapIfPresent String.toInt
            )
        |> with (Option.flag "stat")
        |> OptionsParser.endWith
            (Option.optionalPositionalArg "revision range")
        |> OptionsParser.finally
            (Option.restArgs "rest args")


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


type alias Flags =
    List String


init : Flags -> ( Model, Cmd Msg )
init argv =
    let
        matchResult : Program.RunResult GitOptionsParser
        matchResult =
            Program.run cli argv

        cmd =
            case matchResult of
                Program.SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.ExitStatus.Failure ->
                            Ports.printAndExitFailure message

                        Cli.ExitStatus.Success ->
                            Ports.printAndExitSuccess message

                Program.CustomMatch msg ->
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


init2 : (options -> Cmd msg) -> Program.Program options -> List String -> ( (), Cmd msg )
init2 match program argv =
    let
        matchResult : Program.RunResult options
        matchResult =
            Program.run program argv

        cmd =
            case matchResult of
                Program.SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.ExitStatus.Failure ->
                            Ports.printAndExitFailure message

                        Cli.ExitStatus.Success ->
                            Ports.printAndExitSuccess message

                Program.CustomMatch msg ->
                    match msg
    in
    ( (), cmd )


type alias Model =
    ()


type alias Msg =
    ()


type alias CliProgramOptions msg =
    { printAndExitFailurePort : String -> Cmd msg
    , printAndExitSuccessPort : String -> Cmd msg
    }


cliProgram : Program.Program options -> CliProgramOptions msg -> Platform.Program (List String) () msg
cliProgram cliProgram options =
    Platform.programWithFlags
        { init =
            init2
                (\msg ->
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
                )
                cli
        , update = \msg model -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


main : Platform.Program Flags Model Msg
main =
    cliProgram
        cli
        { printAndExitFailurePort = Ports.printAndExitFailure
        , printAndExitSuccessPort = Ports.printAndExitSuccess
        }
