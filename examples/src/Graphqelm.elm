port module Graphqelm exposing (main)

import Cli
import Cli.Command as Command exposing (Command, with)
import Cli.Spec as Spec
import Cli.Validate
import Json.Decode exposing (..)


type GraphqelmCommand
    = PrintVersion
    | PrintHelp
    | NoOp
    | FromUrl String (Maybe String) (Maybe String) Bool (List String)
    | FromFile String (Maybe String) (Maybe String) Bool


cli : List (Command GraphqelmCommand)
cli =
    [ Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.toCommand
    , Command.build PrintHelp
        |> Command.expectFlag "help"
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
        msg =
            flags
                |> List.drop 2
                |> Cli.try cli

        toPrint =
            case msg |> Maybe.withDefault (Ok NoOp) of
                Ok PrintVersion ->
                    "You are on version 3.1.4"

                Ok PrintHelp ->
                    Cli.helpText "graphqelm" cli

                Ok NoOp ->
                    "\nNo matching command...\n\nUsage:\n\n"
                        ++ Cli.helpText "graphqelm" cli

                Ok (FromUrl url base outputPath excludeDeprecated headers) ->
                    "...fetching from url " ++ url ++ "\noptions: " ++ toString ( url, base, outputPath, excludeDeprecated, headers )

                Ok (FromFile file base outputPath excludeDeprecated) ->
                    "...fetching from file " ++ file ++ "\noptions: " ++ toString ( base, outputPath, excludeDeprecated )

                Err validationErrors ->
                    "Validation errors:\n\n"
                        ++ (validationErrors
                                |> List.map
                                    (\{ name, invalidReason, valueAsString } ->
                                        "`"
                                            ++ name
                                            ++ "` failed a validation. "
                                            ++ invalidReason
                                            ++ "\nValue was:\n"
                                            ++ valueAsString
                                    )
                                |> String.join "\n"
                           )
    in
    ( (), print toPrint )


port print : String -> Cmd msg


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
