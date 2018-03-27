port module Simple exposing (main)

import Json.Decode exposing (..)


type alias Flags =
    List String


type alias Model =
    ()


type Msg
    = PrintVersion
    | PrintHelp


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( (), print "init" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toPrint =
            case msg of
                PrintVersion ->
                    "You are on version 3.1.4"

                PrintHelp ->
                    "asdf"
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
