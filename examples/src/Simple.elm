module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports


programConfig : Program.Config GreetOptions
programConfig =
    Program.config { version = "1.2.3" }
        |> Program.add
            (OptionsParser.build GreetOptions
                |> OptionsParser.with (Option.requiredKeywordArg "name")
                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
            )


type alias GreetOptions =
    { name : String
    , maybeGreeting : Maybe String
    }


init : GreetOptions -> Cmd Never
init { name, maybeGreeting } =
    maybeGreeting
        |> Maybe.withDefault "Hello"
        |> (++) (" " ++ name ++ "!")
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
