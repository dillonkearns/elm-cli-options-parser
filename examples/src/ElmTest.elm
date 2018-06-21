module ElmTest exposing (main)

import Cli
import Cli.Command as Command exposing (Command, with)
import Cli.Spec as Spec
import Json.Decode exposing (..)
import Ports


type ElmTestCommand
    = Init ()
    | RunTests (Maybe Int) (Maybe Int) (Maybe String) (Maybe String) Bool (List String)
    | PrintHelp
    | PrintVersion


cli : List (Command ElmTestCommand)
cli =
    [ Command.subCommand "init" Init
        |> Command.hardcoded ()
        |> Command.toCommand
    , Command.build RunTests
        |> with
            (Spec.optionalKeywordArg "fuzz" |> Spec.validateMapMaybe String.toInt)
        |> with
            (Spec.optionalKeywordArg "seed" |> Spec.validateMapMaybe String.toInt)
        |> with (Spec.optionalKeywordArg "compiler")
        |> with (Spec.optionalKeywordArg "add-dependencies")
        |> with (Spec.flag "watch")
        |> Command.captureRestOperands "TESTFILES"
    , Command.build PrintHelp
        |> Command.expectFlag "help"
        |> Command.toCommand
    , Command.build PrintVersion
        |> Command.expectFlag "version"
        |> Command.toCommand
    ]


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
            Cli.try cli flags

        toPrint =
            case matchResult of
                Cli.NoMatch ->
                    "\nNo matching command...\n\nUsage:\n\n"
                        ++ Cli.helpText "elm-test" cli

                Cli.ValidationErrors validationErrors ->
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

                Cli.Match msg ->
                    case msg of
                        Init () ->
                            "Initializing test suite..."

                        RunTests maybeFuzz maybeSeed maybeCompilerPath maybeDependencies watch testFiles ->
                            [ "Running the following test files: " ++ toString testFiles |> Just
                            , "with watch: " ++ toString watch |> Just
                            , maybeFuzz |> Maybe.map (\fuzz -> "with fuzz: " ++ toString fuzz)
                            , maybeSeed |> Maybe.map (\seed -> "with seed: " ++ toString seed)
                            , maybeCompilerPath |> Maybe.map (\compilerPath -> "with compiler: " ++ toString compilerPath)
                            , maybeDependencies |> Maybe.map (\dependencies -> "with dependencies: " ++ toString dependencies)
                            ]
                                |> List.filterMap identity
                                |> String.join "\n"

                        PrintHelp ->
                            Cli.helpText "elm-test" cli

                        PrintVersion ->
                            "You are on version 3.1.4"
    in
    ( (), Ports.print toPrint )


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
