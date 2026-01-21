module Graphqelm exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser, with)
import Cli.Program as Program
import Cli.Validate
import Ports


type CliOptions
    = FromUrl String (Maybe String) (Maybe String) Bool (List String)
    | FromFile String (Maybe String) (Maybe String) Bool


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build FromUrl
                |> with (Option.requiredPositionalArg "url")
                |> with baseOption
                |> with (Option.optionalKeywordArg "output")
                |> with (Option.flag "excludeDeprecated")
                |> with (Option.keywordArgList "header")
                |> OptionsParser.withDescription "generate files based on the schema at `url`"
            )
        |> Program.add
            (OptionsParser.build FromFile
                |> with (Option.requiredKeywordArg "introspection-file")
                |> with baseOption
                |> with (Option.optionalKeywordArg "output")
                |> with (Option.flag "excludeDeprecated")
            )


baseOption : Option.Option (Maybe String) (Maybe String) Option.BeginningOption
baseOption =
    Option.optionalKeywordArg "base"
        |> Option.validateIfPresent
            (Cli.Validate.regex "^[A-Z][A-Za-z_]*(\\.[A-Z][A-Za-z_]*)*$")


type alias Flags =
    Program.FlagsIncludingArgv {}


init : Flags -> CliOptions -> Cmd Never
init flags msg =
    (case msg of
        FromUrl url base outputPath excludeDeprecated headers ->
            "...fetching from url " ++ url ++ "\noptions: " ++ Debug.toString { url = url, base = base, outputPath = outputPath, excludeDeprecated = excludeDeprecated, headers = headers }

        FromFile file base outputPath excludeDeprecated ->
            "...fetching from file " ++ file ++ "\noptions: " ++ Debug.toString ( base, outputPath, excludeDeprecated )
    )
        |> Ports.print


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , config = program
        }
