module Simple exposing (main)

import Cli
import Cli.Command as Command
import Json.Decode exposing (..)
import Ports


cli : List (Command.Command Msg)
cli =
    [ Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.toCommand
    , Command.build PrintHelp
        |> Command.expectFlag "help"
        |> Command.toCommand
    , Command.build Greet
        |> Command.with (Command.requiredKeywordArg "name")
        |> Command.with (Command.optionalKeywordArg "greeting")
        |> Command.toCommand
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

        toPrint =
            case msg |> Maybe.withDefault (Ok NoOp) of
                Ok PrintVersion ->
                    "You are on version 3.1.4"

                Ok PrintHelp ->
                    Cli.helpText "greet" cli

                Ok NoOp ->
                    "\nNo matching command...\n\nUsage:\n\n"
                        ++ Cli.helpText "simple" cli

                Ok (Greet name maybePrefix) ->
                    case maybePrefix of
                        Just greeting ->
                            greeting ++ " " ++ name ++ "!"

                        Nothing ->
                            "Hello " ++ name ++ "!"

                Err validationErrors ->
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
    in
    ( (), Ports.print toPrint )


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
