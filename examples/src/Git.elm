module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (with)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports


type CliOptions
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


program : Program.Program CliOptions
program =
    Program.program { version = "1.2.3" }
        |> Program.add
            (OptionsParser.buildSubCommand "init" Init
                |> OptionsParser.withDoc "initialize a git repository"
            )
        |> Program.add
            (OptionsParser.buildSubCommand "clone" Clone
                |> with (Option.positionalArg "repository")
            )
        |> Program.add (OptionsParser.map Log logOptionsParser)


logOptionsParser : OptionsParser.ActualOptionsParser LogOptions BuilderState.Terminal
logOptionsParser =
    OptionsParser.buildSubCommand "log" LogOptions
        |> with (Option.optionalKeywordArg "author")
        |> with
            (Option.optionalKeywordArg "max-count"
                |> Option.validateMapIfPresent String.toInt
            )
        |> with (Option.flag "stat")
        |> OptionsParser.optionalPositionalArg
            (Option.optionalPositionalArg "revision range")
        |> OptionsParser.restArgs
            (Option.restArgs "rest args")


init : CliOptions -> Cmd msg
init msg =
    (case msg of
        Init ->
            "Initializing test suite..."

        Clone url ->
            "Cloning `" ++ url ++ "`..."

        Log options ->
            [ "Logging..." |> Just
            , options.maybeAuthorPattern |> Maybe.map (\authorPattern -> "authorPattern: " ++ authorPattern)
            , options.maybeMaxCount |> Maybe.map (\maxCount -> "maxCount: " ++ toString maxCount)
            , "stat: " ++ toString options.statisticsMode |> Just
            , options.maybeRevisionRange |> Maybe.map (\revisionRange -> "revisionRange: " ++ toString revisionRange)
            ]
                |> List.filterMap identity
                |> String.join "\n"
    )
        |> Ports.print


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


main : Program.ProgramNew msg
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , program = program
        }
