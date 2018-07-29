module Simple exposing (main)

import Cli.OptionsParser as OptionsParser
import Cli.ExitStatus
import Cli.Option as Option
import Cli.Program
import Json.Decode exposing (..)
import Ports


cli : Cli.Program.Program Msg
cli =
    { programName = "graphqelm"
    , optionsParsers = optionsParsers
    , version = "1.2.3"
    }


optionsParsers : List (OptionsParser.TerminalOptionsParser Msg)
optionsParsers =
    [ OptionsParser.build Greet
        |> OptionsParser.with (Option.requiredKeywordArg "name")
        |> OptionsParser.with (Option.optionalKeywordArg "greeting")
        |> OptionsParser.end
    ]


dummy : Decoder String
dummy =
    Json.Decode.string


type alias Flags =
    List String


type alias Model =
    ()


type Msg
    = Greet String (Maybe String)


init : Flags -> ( Model, Cmd Msg )
init argv =
    let
        matchResult =
            Cli.Program.run cli argv

        toPrint =
            case matchResult of
                Cli.Program.SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.ExitStatus.Failure ->
                            Ports.printAndExitFailure message

                        Cli.ExitStatus.Success ->
                            Ports.printAndExitSuccess message

                Cli.Program.CustomMatch msg ->
                    case msg of
                        Greet name maybePrefix ->
                            case maybePrefix of
                                Just greeting ->
                                    greeting ++ " " ++ name ++ "!" |> Ports.print

                                Nothing ->
                                    "Hello " ++ name ++ "!" |> Ports.print
    in
    ( (), toPrint )


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
