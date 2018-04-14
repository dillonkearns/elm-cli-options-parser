module Cli.Validate exposing (regex)

import Command
import Regex


regex : String -> String -> Command.ValidationResult
regex regexPattern checkString =
    if Regex.contains (Regex.regex regexPattern) checkString then
        Command.Valid
    else
        Command.Invalid ("Must be of form /" ++ regexPattern ++ "/")
