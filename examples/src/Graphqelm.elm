module Main exposing (main)

import Cli
import Cli.Command as Command exposing (Command, with)
import Cli.Spec as Spec
import Cli.Validate
import Json.Decode exposing (..)
import Ports


type GraphqelmCommand
    = PrintVersion
    | FromUrl String (Maybe String) (Maybe String) Bool (List String)
    | FromFile String (Maybe String) (Maybe String) Bool


cli : List (Command GraphqelmCommand)
cli =
    [ Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.toCommand
    , Command.buildWithDoc FromUrl "generate files based on the schema at `url`"
        |> with (Spec.positionalArg "url")
        |> with baseOption
        |> with (Spec.optionalKeywordArg "output")
        |> with (Spec.flag "excludeDeprecated")
        |> with (Spec.keywordArgList "header")
        |> Command.toCommand
    , Command.build FromFile
        |> with (Spec.requiredKeywordArg "introspection-file")
        |> with baseOption
        |> with (Spec.optionalKeywordArg "output")
        |> with (Spec.flag "excludeDeprecated")
        |> Command.toCommand
    ]


baseOption : Spec.CliSpec (Maybe String) (Maybe String)
baseOption =
    Spec.optionalKeywordArg "base"
        |> Spec.validateIfPresent
            (Cli.Validate.regex "^[A-Z][A-Za-z_]*(\\.[A-Z][A-Za-z_]*)*$")


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


type alias Flags =
    List String


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        matchResult =
            Cli.execute "graphqelm" cli flags

        toPrint =
            case matchResult of
                Cli.SystemMessage exitStatus message ->
                    case exitStatus of
                        Cli.Failure ->
                            Ports.printAndExitFailure message

                        Cli.Success ->
                            Ports.printAndExitSuccess message

                Cli.CustomMatch msg ->
                    (case msg of
                        PrintVersion ->
                            "You are on version 3.1.4"

                        FromUrl url base outputPath excludeDeprecated headers ->
                            "...fetching from url " ++ url ++ "\noptions: " ++ toString ( url, base, outputPath, excludeDeprecated, headers )

                        FromFile file base outputPath excludeDeprecated ->
                            "...fetching from file " ++ file ++ "\noptions: " ++ toString ( base, outputPath, excludeDeprecated )
                    )
                        |> Ports.print
    in
    ( (), toPrint )


type alias Model =
    ()


type alias Msg =
    ()


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
