port module Simple exposing (main)

import Cli
import Cli.Command as Command
import Json.Decode exposing (..)


dummy : Decoder String
dummy =
    Json.Decode.string


type alias Flags =
    List String


type alias Model =
    ()


type Msg
    = PrintVersion
    | PrintHelp
    | NoOp
    | Greet String (Maybe String)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        msg =
            flags
                |> List.drop 2
                |> Cli.try
                    cli
    in
    update (msg |> Maybe.withDefault NoOp) ()


cli : List (Command.Command Msg)
cli =
    [ Command.build PrintVersion |> Command.expectFlag "version" |> Command.toCommand
    , Command.build PrintHelp |> Command.expectFlag "help" |> Command.toCommand
    , Command.build Greet
        |> Command.with (Command.requiredKeywordArg "name")
        |> Command.with (Command.optionalKeywordArg "greeting")
        |> Command.toCommand
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toPrint =
            case msg of
                PrintVersion ->
                    "You are on version 3.1.4"

                PrintHelp ->
                    Cli.helpText "greet" cli

                NoOp ->
                    "No matching command"

                Greet name maybePrefix ->
                    case maybePrefix of
                        Just greeting ->
                            greeting ++ " " ++ name ++ "!"

                        Nothing ->
                            "Hello " ++ name ++ "!"
    in
    ( (), print toPrint )


port print : String -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
