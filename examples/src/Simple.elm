module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports


type Verbosity
    = Quiet
    | Verbose


type alias CliOptions =
    { verbosity : Verbosity
    }


programConfig : Program.Config CliOptions
programConfig =
    Program.config { version = "1.2.3" }
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.flag "verbose"
                        |> Option.mapFlag { present = Verbose, absent = Quiet }
                    )
            )


init : CliOptions -> Cmd Never
init _ =
    Nothing
        |> Maybe.withDefault "Hello"
        |> (\greeting -> greeting ++ " " ++ "" ++ "!")
        |> Ports.print


dummy : Decoder String
dummy =
    Json.Decode.string


main : Program.StatelessProgram Never
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        }
