module Cli.Validate exposing (ValidationResult(..), predicate, regex)

{-|

@docs predicate, ValidationResult, regex

-}

import Regex


{-| -}
type ValidationResult
    = Valid
    | Invalid String


{-| Turns a predicate function into a validate function.

    import Cli.Option as Option
    import Cli.Validate as Validate

    isEven : Int -> Bool
    isEven n =
        n % 2 == 0

    pairsOption : Option.Option (Maybe String) (Maybe Int)
    pairsOption =
        Option.optionalKeywordArg "pair-programmers"
            |> Option.validateMapIfPresent String.toInt
            |> Option.validateIfPresent
                (Validate.predicate "Must be even" isEven)

-}
predicate : String -> (a -> Bool) -> (a -> ValidationResult)
predicate message predicate =
    predicate
        >> (\boolResult ->
                if boolResult then
                    Valid
                else
                    Invalid message
           )


{-| smoething

     Option.optionalKeywordArg "base"
        |> Option.validateIfPresent
          (Cli.Validate.regex "^[A-Z][A-Za-z_]*(\\.[A-Z][A-Za-z_]*)*$")

-}
regex : String -> String -> ValidationResult
regex regexPattern checkString =
    if Regex.contains (Regex.regex regexPattern) checkString then
        Valid
    else
        Invalid ("Must be of form /" ++ regexPattern ++ "/")
