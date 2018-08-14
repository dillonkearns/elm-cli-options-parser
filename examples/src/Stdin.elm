port module Stdin exposing (Event(..), subscriptions)


port onStdinLine : (String -> msg) -> Sub msg


port onStdinClosed : (() -> msg) -> Sub msg


type Event
    = Line String
    | Closed


subscriptions : Sub Event
subscriptions =
    Sub.batch
        [ onStdinLine Line
        , onStdinClosed (\() -> Closed)
        ]
