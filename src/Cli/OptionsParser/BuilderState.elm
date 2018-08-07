module Cli.OptionsParser.BuilderState exposing (AnyOptions, EndOptionsOnly, Terminal)

{-|

@docs AnyOptions, EndOptionsOnly, Terminal

-}


{-| A state where you can add any options (beginning, middle, or terminal)
-}
type AnyOptions
    = AnyOptions


{-| A state where you can add anything but beginning options (i.e. middle or terminal)
-}
type EndOptionsOnly
    = EndOptionsOnly


{-| A state where you can no longer add any options
-}
type Terminal
    = Terminal
