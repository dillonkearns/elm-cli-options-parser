module Cli.UsageSpec exposing (Option(..), UsageSpec(..))

import Occurences exposing (Occurences)


type Option
    = Flag String
    | OptionWithStringArg String


type UsageSpec
    = Option Option Occurences
    | Operand String
    | RestArgs String
