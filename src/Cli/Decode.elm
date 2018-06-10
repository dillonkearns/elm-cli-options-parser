module Cli.Decode exposing (Decoder(Decoder), ProcessingError(..), ValidationError, decoder, map)


type alias ValidationError =
    { name : String
    , invalidReason : String
    , valueAsString : String
    }


type ProcessingError
    = MatchError String
    | UnrecoverableValidationError ValidationError


type Decoder decodesFrom decodesTo
    = Decoder (decodesFrom -> Result String ( List ValidationError, decodesTo ))


decoder : Decoder a a
decoder =
    Decoder (\value -> Ok ( [], value ))


map : (a -> b) -> Decoder from a -> Decoder from b
map mapFunction (Decoder function) =
    Decoder (function >> (\fn -> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value )) fn))
