module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (with)
import Cli.OptionsParser.BuilderState
import Cli.Program as Program
import Cli.Validate
import Ports


programConfig : Program.Config GreetOptions
programConfig =
    Program.config { version = "1.2.3" }
        |> Program.add validateParser


capitalizedNameRegex : String
capitalizedNameRegex =
    "[A-Z][A-Za-z]*"


validateParser : OptionsParser.OptionsParser GreetOptions Cli.OptionsParser.BuilderState.AnyOptions
validateParser =
    OptionsParser.build GreetOptions
        |> with
            (Option.requiredKeywordArg "name"
                |> Option.validate (Cli.Validate.regex capitalizedNameRegex)
            )
        |> with
            (Option.optionalKeywordArg "age"
                |> Option.validateMapIfPresent (String.toInt >> maybeToResult)
            )


type alias GreetOptions =
    { name : String
    , maybeAge : Maybe Int
    }


init : Flags -> GreetOptions -> Cmd Never
init flags { name, maybeAge } =
    maybeAge
        |> Maybe.map (\age -> name ++ " is " ++ String.fromInt age ++ " years old")
        |> Maybe.withDefault ("Hello " ++ name ++ "!")
        |> Ports.print


type alias Flags =
    Program.FlagsIncludingArgv {}


maybeToResult : Maybe value -> Result String value
maybeToResult maybe =
    case maybe of
        Just value ->
            Ok value

        Nothing ->
            Err "Could not convert."


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        }
