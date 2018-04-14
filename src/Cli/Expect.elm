module Cli.Expect exposing (Expectation, flag)

import Cli.Decode
import Cli.Unit exposing (CliUnit(..))
import Cli.UsageSpec exposing (Option(..), UsageSpec(..))
import Occurences exposing (Occurences(Optional))
import Parser


type alias Expectation =
    CliUnit () ()


flag : String -> CliUnit Bool Bool
flag flagName =
    CliUnit
        (\{ options } ->
            if
                options
                    |> List.member (Parser.ParsedOption flagName Parser.Flag)
            then
                Ok True
            else
                Ok False
        )
        (Option (Flag flagName) Optional)
        Cli.Decode.decoder
