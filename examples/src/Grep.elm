module Grep exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
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


buildCliOptions : Bool -> Regex -> CaseSensitivity -> CliOptions
buildCliOptions countMode regex caseSensitivity =
    { countMode = countMode
    , pattern = regex |> applyCaseSensitivity caseSensitivity
    }


applyCaseSensitivity : CaseSensitivity -> Regex -> Regex
applyCaseSensitivity caseSensitivity regex =
    case caseSensitivity of
        IgnoreCase ->
            -- Regex.caseInsensitive regex
            regex

        MatchCase ->
            regex


type CaseSensitivity
    = IgnoreCase
    | MatchCase


programConfig : Program.Config CliOptions
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build buildCliOptions
                |> OptionsParser.with
                    (Option.flag "count")
                |> OptionsParser.with
                    (Option.requiredPositionalArg "pattern"
                        |> Option.validateMap
                            (Regex.fromString
                                >> (\maybeRegex ->
                                        case maybeRegex of
                                            Just regex ->
                                                Ok regex

                                            Nothing ->
                                                Err "Invalid regex."
                                   )
                            )
                    )
                |> OptionsParser.with
                    (Option.flag "ignore-case"
                        |> Option.mapFlag
                            { present = IgnoreCase
                            , absent = MatchCase
                            }
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
                            |> String.fromInt
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
                        ( model, Cmd.none )


subscriptions : CliOptions -> a -> Sub Msg
subscriptions _ model =
    Sub.map OnStdin Stdin.subscriptions


incrementMatchCount : { model | matchCount : Int } -> { model | matchCount : Int }
incrementMatchCount model =
    { model | matchCount = model.matchCount + 1 }


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
