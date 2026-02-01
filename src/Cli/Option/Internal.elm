module Cli.Option.Internal exposing
    ( DataGrabber
    , InnerOption
    , Option(..)
    , OptionMeta
    )

import Cli.Decode
import Cli.UsageSpec exposing (UsageSpec)
import Tokenizer


type Option from to constraints
    = Option (InnerOption from to)


type alias InnerOption from to =
    { dataGrabber : DataGrabber from
    , usageSpec : UsageSpec
    , decoder : Cli.Decode.Decoder from to
    , meta : OptionMeta
    }


{-| Metadata for an option that can be set via withMissingMessage.
-}
type alias OptionMeta =
    { missingMessage : Maybe String
    }


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec
    , operands : List String
    , options : List Tokenizer.ParsedOption
    , operandsSoFar : Int
    }
    -> Result Cli.Decode.ProcessingError decodesTo
