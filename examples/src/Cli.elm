module Cli exposing (helpText, try)

import Command exposing (Command)


try : List (Command msg) -> List String -> Maybe msg
try commands argv =
    commands
        |> List.map (Command.tryMatch argv)
        |> oneOf


oneOf : List (Maybe a) -> Maybe a
oneOf =
    List.foldl
        (\x acc ->
            if acc /= Nothing then
                acc
            else
                x
        )
        Nothing


helpText : String -> List (Command msg) -> String
helpText programName cli =
    cli
        |> List.map (Command.synopsis programName)
        |> String.join "\n"
