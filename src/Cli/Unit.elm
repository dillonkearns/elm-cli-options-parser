module Cli.Unit exposing (CliUnit(..))

import Cli.Decode
import Cli.UsageSpec exposing (..)
import Parser


type CliUnit from to
    = CliUnit (DataGrabber from) UsageSpec (Cli.Decode.Decoder from to)


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec, operands : List String, options : List Parser.ParsedOption, operandsSoFar : Int } -> Result String decodesTo
