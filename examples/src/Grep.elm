module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports
import Regex exposing (Regex)


type Msg
    = OnStdin String


type Verbosity
    = Quiet
    | Verbose


type alias CliOptions =
    { verbosity : Verbosity
    , pattern : Regex
    }


programConfig : Program.Config CliOptions
programConfig =
    Program.config { version = "1.2.3" }
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.flag "verbose"
                        |> Option.mapFlag
                            { present = Verbose
                            , absent = Quiet
                            }
                    )
                |> OptionsParser.with
                    (Option.requiredPositionalArg "pattern"
                        |> Option.map Regex.regex
                    )
            )


type alias Model =
    CliOptions


init : Program.FlagsIncludingArgv {} -> CliOptions -> ( Model, Cmd Msg )
init flags cliOptions =
    ( cliOptions, Cmd.none )


dummy : Decoder String
dummy =
    Json.Decode.string


subscriptions : a -> Sub Msg
subscriptions model =
    Ports.stdin OnStdin


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnStdin line ->
            ( model
            , if
                line
                    |> Regex.contains model.pattern
              then
                "matches: "
                    ++ line
                    |> Ports.print
              else
                "no match"
                    |> Ports.print
            )


main : Program.StatefulProgram Model Msg CliOptions {}
main =
    Program.stateful
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        , subscriptions = subscriptions
        , update = update
        }
