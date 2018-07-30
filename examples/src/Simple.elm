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
            (OptionsParser.build GreetOptions
                |> OptionsParser.with (Option.requiredKeywordArg "name")
                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                |> OptionsParser.end
            )


dummy : Decoder String
dummy =
    Json.Decode.string


type alias GreetOptions =
    { name : String
    , maybeGreeting : Maybe String
    }


init : GreetOptions -> Cmd Never
init { name, maybeGreeting } =
    (case maybeGreeting of
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
