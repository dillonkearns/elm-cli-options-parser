port module Graphqelm exposing (main)

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
    | FromUrl String (Maybe String) (Maybe String) Bool
    | FromFile String (Maybe String) (Maybe String) Bool


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
    [ Command.build PrintVersion |> Command.expectFlag "version"
    , Command.build PrintHelp |> Command.expectFlag "help"
    , Command.build FromUrl
        |> Command.expectOperand "url"
        |> Command.optionalOptionWithStringArg "base"
        |> Command.optionalOptionWithStringArg "output"
        |> Command.withFlag "excludeDeprecated"
    , Command.build FromFile
        |> Command.optionWithStringArg "introspection-file"
        |> Command.optionalOptionWithStringArg "base"
        |> Command.optionalOptionWithStringArg "output"
        |> Command.withFlag "excludeDeprecated"
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toPrint =
            case msg of
                PrintVersion ->
                    "You are on version 3.1.4"

                PrintHelp ->
                    Cli.helpText "graphqelm" cli

                NoOp ->
                    "\nNo matching command...\n\nUsage:\n\n"
                        ++ Cli.helpText "graphqelm" cli

                FromUrl url base outputPath excludeDeprecated ->
                    "...fetching from url " ++ url ++ "\noptions: " ++ toString ( url, base, outputPath, excludeDeprecated )

                FromFile file base outputPath excludeDeprecated ->
                    "...fetching from file " ++ file ++ "\noptions: " ++ toString ( base, outputPath, excludeDeprecated )
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
