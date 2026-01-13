module Cli.Option.Internal exposing
    ( Option(..)
    , InnerOption
    , DataGrabber
    )

import Cli.Decode
import Cli.UsageSpec exposing (UsageSpec)
import Tokenizer


type Option from to middleOrEnding
    = Option (InnerOption from to)


type alias InnerOption from to =
    { dataGrabber : DataGrabber from
    , usageSpec : UsageSpec
    , decoder : Cli.Decode.Decoder from to
    }


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec
    , operands : List String
    , options : List Tokenizer.ParsedOption
    , operandsSoFar : Int
    }
    -> Result Cli.Decode.ProcessingError decodesTo
