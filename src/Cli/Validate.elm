module Cli.Validate exposing (predicate, ValidationResult(..), regex, regexWithMessage)

{-| This module contains helper functions for performing validations (see the
"validate..." functions in `Cli.Option`).

@docs predicate, ValidationResult, regex, regexWithMessage

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
        modBy 2 n == 0

    pairsOption : Option.Option (Maybe String) (Maybe Int)
    pairsOption =
        Option.optionalKeywordArg "pair-programmers"
            |> Option.validateMapIfPresent String.toInt
            |> Option.validateIfPresent
                (Validate.predicate "Must be even" isEven)

-}
predicate : String -> (a -> Bool) -> (a -> ValidationResult)
predicate message predicateFunction =
    predicateFunction
        >> (\boolResult ->
                if boolResult then
                    Valid

                else
                    Invalid message
           )


{-| A helper for regex validations.

    programConfig : Program.Config String
    programConfig =
        Program.config
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
    case Regex.fromString regexPattern of
        Nothing ->
            Valid

        Just actualRegex ->
            if Regex.contains actualRegex checkString then
                Valid

            else
                Invalid ("Must be of form /" ++ regexPattern ++ "/")


{-| A helper for regex validations with an additional message.

    programConfig : Program.Config String
    programConfig =
        Program.config
            |> Program.add
                (OptionsParser.build identity
                    |> OptionsParser.with
                        (Option.requiredKeywordArg "name"
                            |> Option.validate
                                (Cli.Validate.regexWithMessage "I expected this to be" "^[A-Z][A-Za-z_]*")
                        )
                )

If the validation fails, the user gets output like this:

```shell
$ ./greet --name john
Validation errors:

`name` failed a validation. I expected this to be matching "^[A-Z][A-Za-z_]*" but got 'john'
Value was:
"john"
```

-}
regexWithMessage : String -> String -> String -> ValidationResult
regexWithMessage message regexPattern checkString =
    case regex regexPattern checkString of
        Valid ->
            Valid

        Invalid _ ->
            Invalid (message ++ " matching \"" ++ regexPattern ++ "\", but got '" ++ checkString ++ "'")
