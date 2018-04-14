port module Graphqelm exposing (main)

import Cli
import Cli.Unit
import Cli.Validate
import Command exposing (with)
import Json.Decode exposing (..)


type CliCommand
    = PrintVersion
    | PrintHelp
    | NoOp
    | FromUrl String (Maybe String) (Maybe String) Bool (List String)
    | FromFile String (Maybe String) (Maybe String) Bool


cli : List (Command.Command CliCommand)
cli =
    [ Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.toCommand
    , Command.build PrintHelp
        |> Command.expectFlag "help"
        |> Command.toCommand
    , Command.buildWithDoc FromUrl "generate files based on the schema at `url`"
        |> with (Cli.Unit.positionalArg "url")
        |> with baseOption
        |> with (Command.optionalKeywordArg "output")
        |> with (Command.flag "excludeDeprecated")
        |> with (Command.keywordArgList "header")
        |> Command.toCommand
    , Command.build FromFile
        |> with (Command.requiredKeywordArg "introspection-file")
        |> with baseOption
        |> with (Command.optionalKeywordArg "output")
        |> with (Command.flag "excludeDeprecated")
        |> Command.toCommand
    ]


baseOption : Cli.Unit.CliUnit (Maybe String) (Maybe String)
baseOption =
    Command.optionalKeywordArg "base"
        |> Command.validateIfPresent
            (Cli.Validate.regex "^[A-Z][A-Za-z_]*(\\.[A-Z][A-Za-z_]*)*$")


dummy : Decoder String
dummy =
    -- this is a workaround for an Elm compiler bug
    Json.Decode.string


type alias Flags =
    List String


type alias Model =
    ()


type alias Msg =
    ()


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


port print : String -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
