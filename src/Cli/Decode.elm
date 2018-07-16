module Cli.Decode exposing (Decoder(Decoder), ProcessingError(..), ValidationError, decoder, map)


type alias ValidationError =
    { name : String
    , invalidReason : String
    , valueAsString : String
    }


type ProcessingError
    = MatchError String
    | UnexpectedOptions (List String)
    | UnrecoverableValidationError ValidationError


type Decoder decodesFrom decodesTo
    = Decoder (decodesFrom -> Result ProcessingError ( List ValidationError, decodesTo ))


decoder : Decoder a a
decoder =
    Decoder (\value -> Ok ( [], value ))


map : (to -> toMapped) -> Decoder from to -> Decoder from toMapped
map mapFunction (Decoder function) =
    Decoder (function >> (\fn -> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value )) fn))
