module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode exposing (..)
import Ports


type CliOptions
    = Hello HelloOptions
    | Goodbye GoodbyeOptions


type alias HelloOptions =
    { name : String
    , maybeHello : Maybe String
    }


type alias GoodbyeOptions =
    { name : String
    , maybeGoodbye : Maybe String
    }


programConfig : Program.Config CliOptions
programConfig =
    Program.config { version = "1.2.3" }
        |> Program.add
            (OptionsParser.buildSubCommand "hello" HelloOptions
                |> OptionsParser.with (Option.requiredKeywordArg "name")
                |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                |> OptionsParser.map Hello
            )
        |> Program.add
            (OptionsParser.buildSubCommand "goodbye" GoodbyeOptions
                |> OptionsParser.with (Option.requiredKeywordArg "name")
                |> OptionsParser.with (Option.optionalKeywordArg "goodbye")
                |> OptionsParser.map Goodbye
            )


init : CliOptions -> Cmd Never
init _ =
    -- maybeHello
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
