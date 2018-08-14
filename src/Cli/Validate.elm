module Cli.Validate exposing (ValidationResult(..), predicate, regex)

{-| This module contains helper functions for performing validations (see the
"validate..." functions in `Cli.Option`).

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


{-| A helper for regex validations.

    programConfig : Program.Config String
    programConfig =
        Program.config { version = "1.2.3" }
            |> Program.add
                (OptionsParser.build identity
                    |> OptionsParser.with
                        (Option.requiredKeywordArg "name"
                            |> Option.validate
                                (Cli.Validate.regex "^[A-Z][A-Za-z_]*")
                        )
                )

If the validation fails, the user gets output like this:

```shell
$ ./greet --name john
Validation errors:

`name` failed a validation. Must be of form /^[A-Z][A-Za-z_]*/
Value was:
"john"
```

-}
regex : String -> String -> ValidationResult
regex regexPattern checkString =
    if Regex.contains (Regex.regex regexPattern) checkString then
        Valid
    else
        Invalid ("Must be of form /" ++ regexPattern ++ "/")
