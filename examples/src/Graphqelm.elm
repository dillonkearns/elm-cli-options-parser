module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser, with)
import Cli.Program as Program
import Cli.Validate
import Json.Decode exposing (..)
import Ports


type CliOptions
    = FromUrl String (Maybe String) (Maybe String) Bool (List String)
    | FromFile String (Maybe String) (Maybe String) Bool


program : Program.Program CliOptions
program =
    Program.config { version = "1.2.3" }
        |> Program.add
            (OptionsParser.build FromUrl
                |> with (Option.positionalArg "url")
                |> with baseOption
                |> with (Option.optionalKeywordArg "output")
                |> with (Option.flag "excludeDeprecated")
                |> with (Option.keywordArgList "header")
                |> OptionsParser.withDoc "generate files based on the schema at `url`"
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


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


init : CliOptions -> Cmd Never
init msg =
    (case msg of
        FromUrl url base outputPath excludeDeprecated headers ->
            "...fetching from url " ++ url ++ "\noptions: " ++ toString ( url, base, outputPath, excludeDeprecated, headers )

        FromFile file base outputPath excludeDeprecated ->
            "...fetching from file " ++ file ++ "\noptions: " ++ toString ( base, outputPath, excludeDeprecated )
    )
        |> Ports.print


main : Program.ProgramNew Never
main =
    Program.stateless
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , program = program
        }
