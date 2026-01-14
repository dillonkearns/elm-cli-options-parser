module Cli.Decode exposing
    ( Decoder
    , MatchErrorDetail(..)
    , ProcessingError(..)
    , ValidationError
    , decodeFunction
    , decoder
    , map
    , mapProcessingError
    , mapValidationErrors
    )


type alias ValidationError =
    { name : String
    , invalidReason : String
    }


{-| Internal type representing why a parser failed to match.
-}
type MatchErrorDetail
    = MissingExpectedFlag { name : String }
    | MissingRequiredPositionalArg { name : String, operandsSoFar : Int }
    | MissingRequiredKeywordArg { name : String }
    | KeywordArgMissingValue { name : String }
    | ExtraOperand
    | MissingSubCommand { expectedSubCommand : String }
    | WrongSubCommand { expectedSubCommand : String, actualSubCommand : String }


type ProcessingError
    = MatchError MatchErrorDetail
    | UnexpectedOptions (List String)
    | UnrecoverableValidationError ValidationError


type Decoder decodesFrom decodesTo
    = Decoder (decodesFrom -> Result ProcessingError ( List ValidationError, decodesTo ))


decodeFunction :
    Decoder decodesFrom decodesTo
    -> decodesFrom
    -> Result ProcessingError ( List ValidationError, decodesTo )
decodeFunction (Decoder decodeFn) =
    decodeFn


decoder : Decoder a a
decoder =
    Decoder (\value -> Ok ( [], value ))


map : (to -> toMapped) -> Decoder from to -> Decoder from toMapped
map mapFunction (Decoder function) =
    Decoder (function >> (\fn -> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value )) fn))


mapProcessingError : (to -> Result ProcessingError toMapped) -> Decoder from to -> Decoder from toMapped
mapProcessingError mapFunction (Decoder function) =
    Decoder
        (function
            >> (\fn ->
                    Result.andThen
                        (\( validationErrors, value ) ->
                            case mapFunction value of
                                Ok mappedValue ->
                                    Ok ( validationErrors, mappedValue )

                                Err error ->
                                    Err error
                        )
                        fn
               )
        )


mapValidationErrors : (to -> Maybe ValidationError) -> Decoder from to -> Decoder from to
mapValidationErrors addValidationErrors (Decoder function) =
    let
        something value =
            case addValidationErrors value of
                Just validationError ->
                    [ validationError ]

                Nothing ->
                    []
    in
    Decoder
        (function
            >> (\fn ->
                    Result.map
                        (\( validationErrors, value ) ->
                            ( validationErrors
                                ++ something value
                            , value
                            )
                        )
                        fn
               )
        )
