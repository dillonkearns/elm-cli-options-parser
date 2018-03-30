module Occurences exposing (Occurences(..), qualifySynopsis)


type Occurences
    = Optional
    | Required
    | ZeroOrMore


qualifySynopsis : Occurences -> String -> String
qualifySynopsis occurences rawSynopsis =
    case occurences of
        Optional ->
            "[" ++ rawSynopsis ++ "]"

        Required ->
            rawSynopsis

        ZeroOrMore ->
            "[" ++ rawSynopsis ++ "]..."
