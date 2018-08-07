module Cli.OptionsParser.BuilderState exposing (AnyOptions, NoBeginningOptions, NoMoreOptions)

{-|

@docs AnyOptions, NoBeginningOptions, NoMoreOptions

-}


{-| A state where you can add any options (beginning, middle, or terminal)
-}
type AnyOptions
    = AnyOptions


{-| A state where you can add anything but beginning options (i.e. middle or terminal)
-}
type NoBeginningOptions
    = NoBeginningOptions


{-| A state where you can no longer add any options
-}
type NoMoreOptions
    = NoMoreOptions
