module Cli.Validate exposing (ValidationResult(..), regex)

import Regex


type ValidationResult
    = Valid
    | Invalid String


regex : String -> String -> ValidationResult
regex regexPattern checkString =
    if Regex.contains (Regex.regex regexPattern) checkString then
        Valid
    else
        Invalid ("Must be of form /" ++ regexPattern ++ "/")
