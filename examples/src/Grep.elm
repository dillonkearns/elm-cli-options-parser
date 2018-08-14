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


type alias Model =
    { matchCount : Int
    }


type alias CliOptions =
    { countMode : Bool
    , pattern : Regex
    }


programConfig : Program.Config CliOptions
programConfig =
    Program.config { version = "1.2.3" }
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.flag "count")
                |> OptionsParser.with
                    (Option.requiredPositionalArg "pattern"
                        |> Option.map Regex.regex
                    )
            )


init : Program.FlagsIncludingArgv {} -> CliOptions -> ( Model, Cmd Msg )
init flags cliOptions =
    ( { matchCount = 0 }, Cmd.none )


update : CliOptions -> Msg -> Model -> ( Model, Cmd Msg )
update cliOptions msg model =
    case msg of
        OnStdin stdinEvent ->
            if cliOptions.countMode then
                case stdinEvent of
                    Stdin.Line line ->
                        ( if Regex.contains cliOptions.pattern line then
                            model |> incrementMatchCount
                          else
                            model
                        , Cmd.none
                        )

                    Stdin.Closed ->
                        ( model
                        , model.matchCount
                            |> toString
                            |> Ports.print
                        )
            else
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


subscriptions : a -> Sub Msg
subscriptions model =
    Sub.map OnStdin Stdin.subscriptions


incrementMatchCount : { model | matchCount : Int } -> { model | matchCount : Int }
incrementMatchCount model =
    { model | matchCount = model.matchCount + 1 }


dummy : Decoder String
dummy =
    Json.Decode.string


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
