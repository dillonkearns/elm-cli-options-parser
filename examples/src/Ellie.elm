port module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode exposing (..)


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
        |> print


dummy : Decoder String
dummy =
    Json.Decode.string


type alias Flags =
    Program.FlagsIncludingArgv {}


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = printAndExitFailure
        , printAndExitSuccess = printAndExitSuccess
        , init = init
        , config = programConfig
        }


port print : String -> Cmd msg


port printAndExitFailure : String -> Cmd msg


port printAndExitSuccess : String -> Cmd msg
