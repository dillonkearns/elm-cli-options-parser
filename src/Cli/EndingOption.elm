module Cli.EndingOption
    exposing
        ( EndingOption(EndingOption)
        , optionalPositionalArg
        )

{-|

@docs EndingOption


## Positional Arguments

@docs optionalPositionalArg

-}

import Cli.Decode
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import List.Extra
import Tokenizer


{-| TODO
-}
type EndingOption from to
    = EndingOption (DataGrabber from) UsageSpec (Cli.Decode.Decoder from to)


type alias DataGrabber decodesTo =
    { usageSpecs : List UsageSpec
    , operands : List String
    , options : List Tokenizer.ParsedOption
    , operandsSoFar : Int
    }
    -> Result Cli.Decode.ProcessingError decodesTo


{-| TODO
-}
optionalPositionalArg : String -> EndingOption (Maybe String) (Maybe String)
optionalPositionalArg operandDescription =
    EndingOption
        (\flagsAndOperands ->
            let
                operandsSoFar : Int
                operandsSoFar =
                    UsageSpec.operandCount flagsAndOperands.usageSpecs
                        - 1

                maybeArg : Maybe String
                maybeArg =
                    flagsAndOperands.operands
                        |> List.Extra.getAt operandsSoFar
            in
            Ok maybeArg
        )
        (UsageSpec.operand operandDescription)
        Cli.Decode.decoder
