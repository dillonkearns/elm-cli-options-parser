module ElmTest exposing (main)

import Cli
import Cli.Command as Command exposing (Command, with)
import Json.Decode exposing (..)
import Ports
import TypoSuggestion exposing (TypoSuggestion)


type ElmTestCommand
    = Init
    | Clone
    | PrintHelp
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
        |> Command.toCommand
    , Command.subCommand "clone" Clone
        |> Command.toCommand
    , Command.build PrintHelp
        |> Command.expectFlag "help"
        |> Command.toCommand
    , Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.toCommand
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


type ExitStatus
    = Success
    | Failure


type StuffToDo matchResult
    = PrintThisAndExit ExitStatus String
    | NothingFromMe matchResult


execute : List (Command msg) -> List String -> StuffToDo msg
execute cli flags =
    let
        matchResult =
            Cli.try cli flags
    in
    case matchResult of
        Cli.NoMatch unexpectedOptions ->
            if unexpectedOptions == [] then
                "\nNo matching command...\n\nUsage:\n\n"
                    ++ Cli.helpText "elm-test" cli
                    |> PrintThisAndExit Failure
            else
                unexpectedOptions
                    |> List.map (TypoSuggestion.toMessage cli)
                    |> String.join "\n"
                    |> PrintThisAndExit Failure

        Cli.ValidationErrors validationErrors ->
            ("Validation errors:\n\n"
                ++ (validationErrors
                        |> List.map
                            (\{ name, invalidReason, valueAsString } ->
                                "`"
                                    ++ name
                                    ++ "` failed a validation. "
                                    ++ invalidReason
                                    ++ "\nValue was:\n"
                                    ++ valueAsString
                            )
                        |> String.join "\n"
                   )
            )
                |> PrintThisAndExit Failure

        Cli.Match msg ->
            msg
                |> NothingFromMe


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        matchResult =
            execute cli flags

        cmd =
            case matchResult of
                PrintThisAndExit exitStatus message ->
                    case exitStatus of
                        Failure ->
                            Ports.printAndExitFailure message

                        Success ->
                            Ports.printAndExitSuccess message

                NothingFromMe msg ->
                    (case msg of
                        Init ->
                            "Initializing test suite..."

                        Clone ->
                            "Cloning..."

                        PrintHelp ->
                            Cli.helpText "elm-test" cli

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
