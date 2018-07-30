module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (with)
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports


program : Program.Program CliOptions
program =
    { programName = "curl"
    , version = "1.2.3"
    }
        |> Program.program
        |> Program.add
            (OptionsParser.build CliOptions
                |> with (Option.positionalArg "url")
            )


type alias CliOptions =
    { url : String
    }


init : CliOptions -> Cmd Never
init { url } =
    "Fetching from url: "
        ++ url
        |> Ports.print


dummy : Decoder String
dummy =
    Json.Decode.string


main : Program.ProgramNew Never
main =
    Program.programNew
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , program = program
        }
