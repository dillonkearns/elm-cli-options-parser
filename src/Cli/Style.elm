module Cli.Style exposing
    ( applyBold
    , applyCyan
    , applyDim
    , applyGreen
    , applyRed
    )

{-| Internal module for ANSI styling utilities.

This module is not exposed in the package API.

Functions take a Bool where True means "use color" and False means "no color".

-}

import Ansi.Color
import Ansi.Font


{-| Apply an ANSI code to text if color is enabled.
-}
applyAnsi : Bool -> (String -> String) -> String -> String
applyAnsi useColor ansiFunc text =
    if useColor then
        ansiFunc text

    else
        text


{-| Apply bold styling.
-}
applyBold : Bool -> String -> String
applyBold useColor =
    applyAnsi useColor Ansi.Font.bold


{-| Apply cyan color for flags and arguments.
-}
applyCyan : Bool -> String -> String
applyCyan useColor =
    applyAnsi useColor (Ansi.Color.fontColor Ansi.Color.cyan)


{-| Apply red color for errors.
-}
applyRed : Bool -> String -> String
applyRed useColor =
    applyAnsi useColor (Ansi.Color.fontColor Ansi.Color.red)


{-| Apply dim styling for hints.
-}
applyDim : Bool -> String -> String
applyDim useColor =
    applyAnsi useColor Ansi.Font.faint


{-| Apply green color for suggestions.
-}
applyGreen : Bool -> String -> String
applyGreen useColor =
    applyAnsi useColor (Ansi.Color.fontColor Ansi.Color.green)
