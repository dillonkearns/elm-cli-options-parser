module Main exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser, with)
import Cli.Program as Program
import Cli.Validate
import Json.Decode exposing (..)
import Ports


type GraphqelmOptionsParser
    = PrintVersion
    | FromUrl String (Maybe String) (Maybe String) Bool (List String)
    | FromFile String (Maybe String) (Maybe String) Bool


program : Program.Program GraphqelmOptionsParser
program =
    { programName = "graphqelm"
    , optionsParsers = optionsParsers
    , version = "1.2.3"
    }


optionsParsers : List (OptionsParser.TerminalOptionsParser GraphqelmOptionsParser)
optionsParsers =
    [ OptionsParser.build PrintVersion
        |> OptionsParser.expectFlag "version"
        |> OptionsParser.end
    , OptionsParser.build FromUrl
        |> with (Option.positionalArg "url")
        |> with baseOption
        |> with (Option.optionalKeywordArg "output")
        |> with (Option.flag "excludeDeprecated")
        |> with (Option.keywordArgList "header")
        |> OptionsParser.end
        |> OptionsParser.withDoc "generate files based on the schema at `url`"
    , OptionsParser.build FromFile
        |> with (Option.requiredKeywordArg "introspection-file")
        |> with baseOption
        |> with (Option.optionalKeywordArg "output")
        |> with (Option.flag "excludeDeprecated")
        |> OptionsParser.end
    ]


baseOption : Option.Option (Maybe String) (Maybe String) Option.MiddleOption
baseOption =
    Option.optionalKeywordArg "base"
        |> Option.validateIfPresent
            (Cli.Validate.regex "^[A-Z][A-Za-z_]*(\\.[A-Z][A-Za-z_]*)*$")


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


type alias Flags =
    List String


init : GraphqelmOptionsParser -> Cmd Never
init msg =
    (case msg of
        PrintVersion ->
            "You are on version 3.1.4"

        FromUrl url base outputPath excludeDeprecated headers ->
            "...fetching from url " ++ url ++ "\noptions: " ++ toString ( url, base, outputPath, excludeDeprecated, headers )

        FromFile file base outputPath excludeDeprecated ->
            "...fetching from file " ++ file ++ "\noptions: " ++ toString ( base, outputPath, excludeDeprecated )
    )
        |> Ports.print


main : Program.ProgramNew Never
main =
    Program.programNew
        { printAndExitFailure = Ports.printAndExitFailure
        , printAndExitSuccess = Ports.printAndExitSuccess
        , init = init
        , program = program
        }
