module Simple exposing (main)

import Cli.Command as Command
import Cli.OptionsParser
import Cli.Spec as Spec
import Json.Decode exposing (..)
import Ports


cli : List (Command.Command Msg)
cli =
    [ Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.withoutRestArgs
    , Command.build Greet
        |> Command.with (Spec.requiredKeywordArg "name")
        |> Command.with (Spec.optionalKeywordArg "greeting")
        |> Command.withoutRestArgs
    ]


dummy : Decoder String
dummy =
    Json.Decode.string


type alias Flags =
    List String


type alias Model =
    ()


type Msg
    = PrintVersion
    | Greet String (Maybe String)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        matchResult =
            Cli.OptionsParser.run "simple" cli flags

        toPrint =
            case matchResult of
                Cli.OptionsParser.SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.OptionsParser.Failure ->
                            Ports.printAndExitFailure message

                        Cli.OptionsParser.Success ->
                            Ports.printAndExitSuccess message

                Cli.OptionsParser.CustomMatch msg ->
                    case msg of
                        PrintVersion ->
                            "You are on version 3.1.4"
                                |> Ports.print

                        Greet name maybePrefix ->
                            case maybePrefix of
                                Just greeting ->
                                    greeting ++ " " ++ name ++ "!" |> Ports.print

                                Nothing ->
                                    "Hello " ++ name ++ "!" |> Ports.print
    in
    ( (), toPrint )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( (), Cmd.none )


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
