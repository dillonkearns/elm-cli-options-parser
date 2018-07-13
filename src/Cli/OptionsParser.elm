module Cli.OptionsParser exposing (ExecuteResult(..), ExitStatus(..), execute)

{-| TODO

@docs ExecuteResult, ExitStatus
@docs execute

-}

import Cli.Command as Command exposing (Command)
import Cli.LowLevel
import TypoSuggestion


{-| TODO
-}
type ExecuteResult match
    = SystemMessage ExitStatus String
    | CustomMatch match


{-| TODO
-}
type ExitStatus
    = Success
    | Failure


{-| TODO
-}
execute : String -> List (Command msg) -> List String -> ExecuteResult msg
execute programName cli flags =
    let
        matchResult =
            Cli.LowLevel.try cli flags
    in
    case matchResult of
        Cli.LowLevel.NoMatch unexpectedOptions ->
            if unexpectedOptions == [] then
                "\nNo matching command...\n\nUsage:\n\n"
                    ++ Cli.LowLevel.helpText "elm-test" cli
                    |> SystemMessage Failure
            else
                unexpectedOptions
                    |> List.map (TypoSuggestion.toMessage cli)
                    |> String.join "\n"
                    |> SystemMessage Failure

        Cli.LowLevel.ValidationErrors validationErrors ->
            ("Validation errors:\n\n"
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
            )
                |> SystemMessage Failure

        Cli.LowLevel.Match msg ->
            msg
                |> CustomMatch

        Cli.LowLevel.ShowHelp ->
            Cli.LowLevel.helpText programName cli
                |> SystemMessage Success
