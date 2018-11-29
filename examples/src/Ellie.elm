port module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (with)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.Program as Program


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


programConfig : Program.Config CliOptions
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.buildSubCommand "init" Init
                |> OptionsParser.withDoc "initialize a git repository"
            )
        |> Program.add
            (OptionsParser.buildSubCommand "clone" Clone
                |> with (Option.requiredPositionalArg "repository")
            )
        |> Program.add (OptionsParser.map Log logOptionsParser)


logOptionsParser : OptionsParser.OptionsParser LogOptions BuilderState.NoMoreOptions
logOptionsParser =
    OptionsParser.buildSubCommand "log" LogOptions
        |> with (Option.optionalKeywordArg "author")
        |> with
            (Option.optionalKeywordArg "max-count"
                |> Option.validateMapIfPresent (String.toInt >> maybeToResult)
            )
        |> with (Option.flag "stat")
        |> OptionsParser.withOptionalPositionalArg
            (Option.optionalPositionalArg "revision range")
        |> OptionsParser.withRestArgs
            (Option.restArgs "rest args")


init : Flags -> CliOptions -> Cmd Never
init flags cliOptions =
    (case cliOptions of
        Init ->
            "Initialized empty Git repository..."

        Clone url ->
            "Cloning `" ++ url ++ "`..."

        Log options ->
            [ "Logging..." |> Just
            , options.maybeAuthorPattern |> Maybe.map (\authorPattern -> "authorPattern: " ++ authorPattern)
            , options.maybeMaxCount |> Maybe.map (\maxCount -> "maxCount: " ++ String.fromInt maxCount)
            , "stat: " ++ Debug.toString options.statisticsMode |> Just
            , options.maybeRevisionRange |> Maybe.map (\revisionRange -> "revisionRange: " ++ Debug.toString revisionRange)
            ]
                |> List.filterMap identity
                |> String.join "\n"
    )
        |> print


maybeToResult : Maybe value -> Result String value
maybeToResult maybe =
    case maybe of
        Just value ->
            Ok value

        Nothing ->
            Err "Could not convert."


type alias Flags =
    Program.FlagsIncludingArgv {}


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = printAndExitFailure
        , printAndExitSuccess = printAndExitSuccess
        , init = init
        , config = programConfig
        }


port print : String -> Cmd msg


port printAndExitFailure : String -> Cmd msg


port printAndExitSuccess : String -> Cmd msg
