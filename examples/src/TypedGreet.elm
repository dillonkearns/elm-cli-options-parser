module TypedGreet exposing (main)

{-| A simple example using `Cli.Option.Typed` for typed options with JSON schema support.

This is the typed equivalent of the `Simple.elm` example. The key difference is
that each option specifies its type via a `CliDecoder`, enabling JSON schema
generation via `Program.toJsonSchema`.

Try it:

    node -e "require('./create-cli')('TypedGreet')" -- --name "World"

    node -e "require('./create-cli')('TypedGreet')" -- --name "World" --greeting "Hi" --times 3

Or with JSON input:

    node -e "require('./create-cli')('TypedGreet')" -- '{"name": "World", "times": 3, "$cli": {}}'

-}

import Cli.Option.Typed as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Ports


type alias GreetOptions =
    { name : String
    , greeting : String
    , times : Int
    }


programConfig : Program.Config GreetOptions
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build GreetOptions
                |> OptionsParser.with (Option.requiredKeywordArg "name" Option.string)
                |> OptionsParser.with
                    (Option.optionalKeywordArg "greeting" Option.string
                        |> Option.withDefault "Hello"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "times" Option.int
                        |> Option.withDefault 1
                    )
            )


init : Flags -> GreetOptions -> Cmd Never
init flags { name, greeting, times } =
    List.repeat times (greeting ++ " " ++ name ++ "!")
        |> String.join "\n"
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
