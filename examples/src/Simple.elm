port module Simple exposing (main)

import Cli
import Command
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        msg =
            flags
                |> List.drop 2
                |> Cli.try
                    [ Command.command PrintVersion (Command.LongOnly "version")
                    , Command.command PrintHelp (Command.LongOnly "help")
                    ]
    in
    update (msg |> Maybe.withDefault NoOp) ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toPrint =
            case msg of
                PrintVersion ->
                    "You are on version 3.1.4"

                PrintHelp ->
                    "Help text here..."

                NoOp ->
                    "No matching command"
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
