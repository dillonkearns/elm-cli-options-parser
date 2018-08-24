module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Ports


type alias GreetOptions =
    { name : String
    , maybeGreeting : Maybe String
    }


programConfig : Program.Config GreetOptions
programConfig =
    Program.config { version = "1.2.3" }
        |> Program.add
            (OptionsParser.build GreetOptions
                |> OptionsParser.with (Option.requiredKeywordArg "name")
                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
            )


init : Flags -> GreetOptions -> Cmd Never
init flags { maybeGreeting, name } =
    maybeGreeting
        |> Maybe.withDefault "Hello"
        |> (\greeting -> greeting ++ " " ++ name ++ "!")
        |> Ports.print


type alias Flags =
    Program.FlagsIncludingArgv {}


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        }
