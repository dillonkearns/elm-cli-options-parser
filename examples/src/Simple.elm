module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports


program : Program.Program GreetOptions
program =
    { programName = "greet"
    , version = "1.2.3"
    }
        |> Program.program
        |> Program.add
            (OptionsParser.build Greet
                |> OptionsParser.with (Option.requiredKeywordArg "name")
                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                |> OptionsParser.end
            )


dummy : Decoder String
dummy =
    Json.Decode.string


type GreetOptions
    = Greet String (Maybe String)


init : GreetOptions -> Cmd Never
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
