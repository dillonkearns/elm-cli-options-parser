module Cli.ColorMode exposing (ColorMode(..), useColor)

{-| Control whether ANSI color codes are included in output.

@docs ColorMode, useColor

-}


{-| Control whether ANSI color codes are included in output.

  - `WithColor` - Include ANSI color codes for styled terminal output
  - `WithoutColor` - Plain text output without any ANSI codes

Typically you detect color support in JavaScript and pass the appropriate mode:

    const useColor = process.stdout.isTTY && !process.env.NO_COLOR;
    Elm.Main.init({
      flags: {
        argv: process.argv,
        versionMessage: "1.0.0",
        colorMode: useColor
      }
    });

-}
type ColorMode
    = WithColor
    | WithoutColor


{-| Convert ColorMode to a boolean for internal use.
-}
useColor : ColorMode -> Bool
useColor colorMode =
    case colorMode of
        WithColor ->
            True

        WithoutColor ->
            False
