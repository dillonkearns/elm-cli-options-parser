module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports
import Regex exposing (Regex)
import Stdin


type Msg
    = OnStdin Stdin.Event


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
    ()


init : Program.FlagsIncludingArgv {} -> CliOptions -> ( Model, Cmd Msg )
init flags cliOptions =
    ( (), Cmd.none )


dummy : Decoder String
dummy =
    Json.Decode.string


subscriptions : a -> Sub Msg
subscriptions model =
    Sub.map OnStdin Stdin.subscriptions


update : CliOptions -> Msg -> Model -> ( Model, Cmd Msg )
update cliOptions msg model =
    case msg of
        OnStdin stdinEvent ->
            case stdinEvent of
                Stdin.Line line ->
                    ( model
                    , if Regex.contains cliOptions.pattern line then
                        Ports.print line
                      else
                        Cmd.none
                    )

                Stdin.Closed ->
                    ( model, Ports.print "Closed stdin..." )


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
