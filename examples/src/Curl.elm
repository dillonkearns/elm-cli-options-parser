module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (with)
import Cli.Program as Program
import Http
import Json.Decode exposing (..)
import Ports


type Msg
    = GotResponse (Result Http.Error String)


type alias Model =
    ()


program : Program.Program CliOptions
program =
    Program.program { version = "1.2.3" }
        |> Program.add
            (OptionsParser.build CliOptions
                |> with (Option.positionalArg "url")
            )


type alias CliOptions =
    { url : String
    }


init : CliOptions -> ( Model, Cmd Msg )
init { url } =
    ( ()
    , Cmd.batch
        [ "Fetching from url: " ++ url |> Ports.print
        , url |> Http.getString |> Http.send GotResponse
        ]
    )


dummy : Decoder String
dummy =
    Json.Decode.string


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotResponse httpResult) model =
    case httpResult of
        Ok httpResponse ->
            ( model, Ports.print httpResponse )

        Err error ->
            ( model, error |> toString |> Ports.print )


main : Program.StatefulProgram Model Msg
main =
    Program.stateful
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , program = program
        , update = update
        , subscriptions = \_ -> Sub.none
        }
