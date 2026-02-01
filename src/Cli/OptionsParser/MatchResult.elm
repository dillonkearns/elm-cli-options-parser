module Cli.OptionsParser.MatchResult exposing
    ( MatchResult(..), NoMatchReason(..)
    , matchResultToMaybe
    )

{-| Result types for CLI option parsing.

@docs MatchResult, NoMatchReason

@docs matchResultToMaybe

-}

import Cli.Decode


{-| The result of attempting to match command-line arguments against an OptionsParser.

  - `Match` - The parser matched. Contains either validation errors or the parsed value.
  - `NoMatch` - The parser did not match. Contains the reason(s) why.

-}
type MatchResult msg
    = Match (Result (List Cli.Decode.ValidationError) msg)
    | NoMatch (List NoMatchReason)


{-| Describes why an OptionsParser failed to match the given arguments.

  - `UnexpectedOption` - User provided a flag/option that isn't recognized (e.g., `--foo` when only `--bar` is defined)
  - `MissingSubCommand` - Parser expects a subcommand but none was provided. Contains the expected subcommand name.
  - `WrongSubCommand` - Parser expects a different subcommand. Contains `{ expected : String, actual : String }`.
  - `MissingRequiredPositionalArg` - A required positional argument was not provided. Contains the argument name and optional custom message.
  - `MissingRequiredKeywordArg` - A required keyword argument like `--name <value>` was not provided. Contains the option name and optional custom message.
  - `MissingExpectedFlag` - Parser requires a specific flag (via `expectFlag`) that wasn't provided. Contains the flag name.
  - `ExtraOperand` - More positional arguments were provided than expected.

-}
type NoMatchReason
    = UnexpectedOption String
    | MissingSubCommand { expectedSubCommand : String }
    | WrongSubCommand { expectedSubCommand : String, actualSubCommand : String }
    | MissingRequiredPositionalArg { name : String, customMessage : Maybe String }
    | MissingRequiredKeywordArg { name : String, customMessage : Maybe String }
    | MissingExpectedFlag { name : String }
    | ExtraOperand


{-| Convert a MatchResult to a Maybe, discarding the NoMatch reasons.
-}
matchResultToMaybe : MatchResult msg -> Maybe (Result (List Cli.Decode.ValidationError) msg)
matchResultToMaybe matchResult =
    case matchResult of
        Match thing ->
            Just thing

        NoMatch _ ->
            Nothing
