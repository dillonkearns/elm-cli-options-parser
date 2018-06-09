module ElmTest exposing (main)

import Cli
import Cli.Command as Command exposing (Command, with)
import Json.Decode exposing (..)
import Ports


type ElmTestCommand
    = Init ()
    | RunTests (List String)


cli : List (Command ElmTestCommand)
cli =
    [ Command.subCommand "init" Init
        |> Command.hardcoded ()
        |> Command.toCommand
    , Command.build RunTests
        |> Command.captureRestOperands "TESTFILES"
    ]


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
            Cli.try cli flags

        toPrint =
            case matchResult of
                Cli.NoMatch ->
                    "\nNo matching command...\n\nUsage:\n\n"
                        ++ Cli.helpText "elm-test" cli

                Cli.ValidationErrors validationErrors ->
                    "Validation errors:\n\n"
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

                Cli.Match msg ->
                    case msg of
                        Init () ->
                            "Initializing test suite..."

                        RunTests testFiles ->
                            "Running the following test files: " ++ toString testFiles
    in
    ( (), Ports.print toPrint )


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
