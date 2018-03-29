module Occurences exposing (Occurences(..), qualifySynopsis)


type Occurences
    = Optional
    | Required


qualifySynopsis : Occurences -> String -> String
qualifySynopsis occurences rawSynopsis =
    case occurences of
        Optional ->
            "[" ++ rawSynopsis ++ "]"

        Required ->
            rawSynopsis
