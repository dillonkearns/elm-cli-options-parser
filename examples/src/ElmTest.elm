module ElmTest exposing (main)

import Cli
import Cli.Command as Command exposing (Command, with)
import Cli.Spec as Spec
import Cli.Validate
import Json.Decode exposing (..)
import Ports


type ElmTestCommand
    = Init ()
    | NoOp


cli : List (Command ElmTestCommand)
cli =
    [ Command.subCommand "init" Init
        |> Command.hardcoded ()
        |> Command.toCommand
    ]


baseOption : Spec.CliSpec (Maybe String) (Maybe String)
baseOption =
    Spec.optionalKeywordArg "base"
        |> Spec.validateIfPresent
            (Cli.Validate.regex "^[A-Z][A-Za-z_]*(\\.[A-Z][A-Za-z_]*)*$")


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


type alias Flags =
    List String


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        msg =
            flags
                |> List.drop 2
                |> Cli.try cli

        toPrint =
            case msg |> Maybe.withDefault (Ok NoOp) of
                Ok (Init ()) ->
                    "Initializing test suite..."

                Ok NoOp ->
                    "\nNo matching command...\n\nUsage:\n\n"
                        ++ Cli.helpText "elm-test" cli

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
