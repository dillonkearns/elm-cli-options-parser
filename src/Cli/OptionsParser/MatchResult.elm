module Cli.OptionsParser.MatchResult exposing
    ( MatchResult(..)
    , matchResultToMaybe
    )

{-| TODO

@docs MatchResult

@docs matchResultToMaybe

-}

import Cli.Decode


{-| TODO
-}
type MatchResult msg
    = Match (Result (List Cli.Decode.ValidationError) msg)
    | NoMatch (List String)


{-| TODO
-}
matchResultToMaybe : MatchResult msg -> Maybe (Result (List Cli.Decode.ValidationError) msg)
matchResultToMaybe matchResult =
    case matchResult of
        Match thing ->
            Just thing

        NoMatch unknownFlags ->
            Nothing
