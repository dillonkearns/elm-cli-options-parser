module Simple exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports


program : Program.Program Msg
program =
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


type Msg
    = Greet String (Maybe String)


init : Msg -> Cmd Never
init msg =
    (case msg of
        Greet name maybePrefix ->
            case maybePrefix of
                Just greeting ->
                    greeting ++ " " ++ name ++ "!"

                Nothing ->
                    "Hello " ++ name ++ "!"
    )
        |> Ports.print


main : Program.ProgramNew Never
main =
    Program.programNew
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , program = program
        }
