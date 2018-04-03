module Cli.Decode exposing (Decoder(Decoder), decoder, map)


type Decoder decodesFrom decodesTo
    = Decoder (decodesFrom -> Result String decodesTo)


decoder : Decoder a a
decoder =
    Decoder (\value -> Ok value)


map : (a -> b) -> Decoder from a -> Decoder from b
map mapFunction (Decoder function) =
    Decoder (function >> (\fn -> Result.map mapFunction fn))
