module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (with)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.Program as Program
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


programConfig : Program.Config CliOptions
programConfig =
    Program.config { version = "1.2.3" }
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


maybeToResult : Maybe value -> Result String value
maybeToResult maybe =
    case maybe of
        Just value ->
            Ok value

        Nothing ->
            Err "Could not convert."


init : Flags -> CliOptions -> Cmd Never
init flags cliOptions =
    (case cliOptions of
        Init ->
            "Initializing test suite..."

        Clone url ->
            "Cloning `" ++ url ++ "`..."

        Log options ->
            [ "Logging..." |> Just
            , options.maybeAuthorPattern |> Maybe.map (\authorPattern -> "authorPattern: " ++ authorPattern)
            , options.maybeMaxCount |> Maybe.map (\maxCount -> "maxCount: " ++ String.fromInt maxCount)
            , "stat: " ++ boolToString options.statisticsMode |> Just
            , options.maybeRevisionRange |> Maybe.map (\revisionRange -> "revisionRange: " ++ revisionRange)
            ]
                |> List.filterMap identity
                |> String.join "\n"
    )
        |> Ports.print


boolToString : Bool -> String
boolToString bool =
    case bool of
        True ->
            "True"

        False ->
            "False"


type alias Flags =
    Program.FlagsIncludingArgv {}


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        }
