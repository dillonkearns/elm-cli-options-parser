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


type alias Msg =
    ()


type InitMsg
    = PrintVersion
    | PrintHelp
    | NoOp
    | FromUrl String (Maybe String) (Maybe String) Bool (List String)
    | FromFile String (Maybe String) (Maybe String) Bool


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        msg =
            flags
                |> List.drop 2
                |> Cli.try cli

        toPrint =
            case msg |> Maybe.withDefault NoOp of
                PrintVersion ->
                    "You are on version 3.1.4"

                PrintHelp ->
                    Cli.helpText "graphqelm" cli

                NoOp ->
                    "\nNo matching command...\n\nUsage:\n\n"
                        ++ Cli.helpText "graphqelm" cli

                FromUrl url base outputPath excludeDeprecated headers ->
                    "...fetching from url " ++ url ++ "\noptions: " ++ toString ( url, base, outputPath, excludeDeprecated, headers )

                FromFile file base outputPath excludeDeprecated ->
                    "...fetching from file " ++ file ++ "\noptions: " ++ toString ( base, outputPath, excludeDeprecated )
    in
    ( (), print toPrint )


cli : List (Command.Command InitMsg)
cli =
    [ Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.toCommand
    , Command.build PrintHelp
        |> Command.expectFlag "help"
        |> Command.toCommand
    , Command.buildWithDoc FromUrl "generate files based on the schema at `url`"
        |> Command.with (Command.requiredOperand "url")
        |> Command.with (Command.optionalOption "base")
        |> Command.with (Command.optionalOption "output")
        |> Command.with (Command.optionalFlag "excludeDeprecated")
        |> Command.with (Command.optionalListOption "header")
        |> Command.toCommand
    , Command.build FromFile
        |> Command.with (Command.requiredOption "introspection-file")
        |> Command.with (Command.optionalOption "base")
        |> Command.with (Command.optionalOption "output")
        |> Command.with (Command.optionalFlag "excludeDeprecated")
        |> Command.toCommand
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


port print : String -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
