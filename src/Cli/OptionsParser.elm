module Cli.OptionsParser exposing
    ( OptionsParser
    , build, buildSubCommand
    , with
    , withOptionalPositionalArg, withRestArgs
    , expectFlag
    , map
    , hardcoded
    , withDoc
    , getSubCommand, getUsageSpecs, synopsis, tryMatch, end, detailedHelp
    )

{-|


## Types

@docs OptionsParser


## Start the Pipeline

You build up an `OptionsParser` similarly to the way you build a decoder using the
[elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest)
pattern. That is, you start the pipeline by giving it a constructor function,
and then for each argument of your constructor function, you have a corresponding

    |> with (Option.someKindOfOption)

in the exact same order.

For example, if we define a type alias for a record with two attributes,
Elm generates a 2-argument constructor function for that record type. Here
Elm gives us a `GreetOptions` function of the type `String -> Maybe String -> GreetOptions`
(this is just a core Elm language feature). That is, if we pass in a `String` and
a `Maybe String` as the 1st and 2nd arguments to the `GreetOptions` function,
it will build up a record of that type.

So in this example, we call `OptionsParser.build` with our `GreetOptions`
constructor function. Then we chain on `with` once for each of those two arguments.
Note that the first `with` will give us a `String`, and the second will give us
a `Maybe String`, so it matches up perfectly with the order of our constructor's
arguments.

    import Cli.Option as Option
    import Cli.OptionsParser as OptionsParser exposing (with)
    import Cli.Program as Program

    type alias GreetOptions =
        { name : String
        , maybeGreeting : Maybe String
        }

    programConfig : Program.Config GreetOptions
    programConfig =
        Program.config
            |> Program.add
                (OptionsParser.build GreetOptions
                    |> with (Option.requiredKeywordArg "name")
                    |> with (Option.optionalKeywordArg "greeting")
                )

@docs build, buildSubCommand


## Adding `Cli.Option.Option`s To The Pipeline

Most options can be chained on using `with`. There are two exceptions,
`restArgs` and `optionalPositionalArg`s. `elm-cli-options-parser` enforces that
they are added in an unambiguous order (see the `Cli.OptionsParser.BuilderState` docs).
So instead of using `with`, you add them with their corresponding `with...`
functions.

    import Cli.Option
    import Cli.OptionsParser as OptionsParser exposing (with)

    type GitOptionsParser
        = Init
        | Log LogOptions -- ...

    type alias LogOptions =
        { maybeAuthorPattern : Maybe String
        , maybeNumberToDisplay : Maybe Int
        }

    logOptionsParser =
        OptionsParser.buildSubCommand "log" LogOptions
            |> with (Option.optionalKeywordArg "author")
            |> with
                (Option.optionalKeywordArg "max-count"
                    |> Option.validateMapIfPresent String.toInt
                )
            |> with (Option.flag "stat")
            |> OptionsParser.withOptionalPositionalArg
                (Option.optionalPositionalArg "revision range")
            |> OptionsParser.withRestArgs
                (Option.restArgs "rest args")


### User Error Message on Invalid Number of Positional Args

The User of the Command-Line Interface will get an error message if there is no
`OptionsParser` that succeeds. And an `OptionsParser` will only succeed if
a valid number of positional arguments is passed in, as defined by these rules:

  - At least the number of required arguments
  - Can be any number greater than that if there are `restArgs`
  - Could be up to as many as (the number of required arguments) + (the number of optional arguments) if there are no rest args

@docs with
@docs withOptionalPositionalArg, withRestArgs

@docs expectFlag


## Mapping and Transforming

@docs map
@docs hardcoded


## Meta-Data

@docs withDoc


## Low-Level Functions

You shouldn't need to use these functions to build a command line utility.

@docs getSubCommand, getUsageSpecs, synopsis, tryMatch, end, detailedHelp

-}

import Cli.Decode
import Cli.Option
import Cli.Option.Internal as Internal
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.OptionsParser.MatchResult
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Occurences exposing (Occurences(..))
import Tokenizer exposing (ParsedOption)


{-| Low-level function, for internal use.
-}
getUsageSpecs : OptionsParser decodesTo builderState -> List UsageSpec
getUsageSpecs (OptionsParser { usageSpecs }) =
    usageSpecs


{-| Low-level function, for internal use.
-}
synopsis : String -> OptionsParser decodesTo builderState -> String
synopsis programName optionsParser =
    optionsParser
        |> (\(OptionsParser record) -> record)
        |> UsageSpec.synopsis programName


{-| Low-level function, for internal use.
Generate detailed help text with Usage line and Options section.
-}
detailedHelp : String -> OptionsParser decodesTo builderState -> String
detailedHelp programName optionsParser =
    optionsParser
        |> (\(OptionsParser record) -> record)
        |> UsageSpec.detailedHelp programName


{-| Low-level function, for internal use.
-}
getSubCommand : OptionsParser cliOptions builderState -> Maybe String
getSubCommand (OptionsParser { subCommand }) =
    subCommand


{-| Low-level function, for internal use.
-}
tryMatch : List String -> OptionsParser cliOptions builderState -> Cli.OptionsParser.MatchResult.MatchResult cliOptions
tryMatch argv ((OptionsParser { usageSpecs, subCommand }) as optionsParser) =
    let
        flagsAndOperands =
            Tokenizer.flagsAndOperands usageSpecs argv
                |> (\record ->
                        case ( subCommand, record.operands ) of
                            ( Nothing, _ ) ->
                                Ok
                                    { options = record.options
                                    , operands = record.operands
                                    , usageSpecs = usageSpecs
                                    }

                            ( Just expectedSubCommandName, actualSubCommand :: remainingOperands ) ->
                                if actualSubCommand == expectedSubCommandName then
                                    Ok
                                        { options = record.options
                                        , operands = remainingOperands
                                        , usageSpecs = usageSpecs
                                        }

                                else
                                    Err
                                        { subCommandError =
                                            Cli.Decode.WrongSubCommand
                                                { expectedSubCommand = expectedSubCommandName
                                                , actualSubCommand = actualSubCommand
                                                }
                                        , options = record.options
                                        }

                            ( Just expectedSubCommandName, [] ) ->
                                Err
                                    { subCommandError =
                                        Cli.Decode.MissingSubCommand
                                            { expectedSubCommand = expectedSubCommandName }
                                    , options = record.options
                                    }
                   )
    in
    case flagsAndOperands of
        Ok actualFlagsAndOperands ->
            let
                parser : OptionsParser cliOptions builderState
                parser =
                    optionsParser
                        |> expectedPositionalArgCountOrFail
                        |> failIfUnexpectedOptions
            in
            case getDecoder parser actualFlagsAndOperands of
                Err error ->
                    case error of
                        Cli.Decode.MatchError matchErrorDetail ->
                            Cli.OptionsParser.MatchResult.NoMatch
                                [ matchErrorDetailToNoMatchReason matchErrorDetail ]

                        Cli.Decode.UnrecoverableValidationError validationError ->
                            Cli.OptionsParser.MatchResult.Match (Err [ validationError ])

                        Cli.Decode.UnexpectedOptions unexpectedOptions ->
                            Cli.OptionsParser.MatchResult.NoMatch
                                (List.map Cli.OptionsParser.MatchResult.UnexpectedOption unexpectedOptions)

                Ok ( [], value ) ->
                    Cli.OptionsParser.MatchResult.Match (Ok value)

                Ok ( validationErrors, _ ) ->
                    Cli.OptionsParser.MatchResult.Match (Err validationErrors)

        Err { subCommandError, options } ->
            -- Include both the subcommand error AND any unexpected options
            let
                unexpectedOptionReasons =
                    unexpectedOptions_ optionsParser options
                        |> List.map Cli.OptionsParser.MatchResult.UnexpectedOption
            in
            Cli.OptionsParser.MatchResult.NoMatch
                (matchErrorDetailToNoMatchReason subCommandError :: unexpectedOptionReasons)


{-| Convert internal MatchErrorDetail to public NoMatchReason.
-}
matchErrorDetailToNoMatchReason : Cli.Decode.MatchErrorDetail -> Cli.OptionsParser.MatchResult.NoMatchReason
matchErrorDetailToNoMatchReason detail =
    case detail of
        Cli.Decode.MissingExpectedFlag { name } ->
            Cli.OptionsParser.MatchResult.MissingExpectedFlag { name = name }

        Cli.Decode.MissingRequiredPositionalArg { name, customMessage } ->
            Cli.OptionsParser.MatchResult.MissingRequiredPositionalArg { name = name, customMessage = customMessage }

        Cli.Decode.MissingRequiredKeywordArg { name, customMessage } ->
            Cli.OptionsParser.MatchResult.MissingRequiredKeywordArg { name = name, customMessage = customMessage }

        Cli.Decode.KeywordArgMissingValue { name } ->
            -- Treat "keyword arg provided without value" same as "missing required keyword arg"
            Cli.OptionsParser.MatchResult.MissingRequiredKeywordArg { name = name, customMessage = Nothing }

        Cli.Decode.ExtraOperand ->
            Cli.OptionsParser.MatchResult.ExtraOperand

        Cli.Decode.MissingSubCommand { expectedSubCommand } ->
            Cli.OptionsParser.MatchResult.MissingSubCommand { expectedSubCommand = expectedSubCommand }

        Cli.Decode.WrongSubCommand { expectedSubCommand, actualSubCommand } ->
            Cli.OptionsParser.MatchResult.WrongSubCommand
                { expectedSubCommand = expectedSubCommand
                , actualSubCommand = actualSubCommand
                }


expectedPositionalArgCountOrFail : OptionsParser cliOptions builderState -> OptionsParser cliOptions builderState
expectedPositionalArgCountOrFail (OptionsParser ({ decoder, usageSpecs } as optionsParser)) =
    OptionsParser
        { optionsParser
            | decoder =
                \({ operands } as stuff) ->
                    if
                        not (UsageSpec.hasRestArgs usageSpecs)
                            && (operands |> List.length)
                            > (usageSpecs
                                |> List.filter UsageSpec.isOperand
                                |> List.length
                              )
                    then
                        Cli.Decode.MatchError Cli.Decode.ExtraOperand |> Err

                    else
                        decoder stuff
        }


getDecoder :
    OptionsParser cliOptions builderState
    ->
        { operands : List String
        , options : List ParsedOption
        , usageSpecs : List UsageSpec
        }
    -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, cliOptions )
getDecoder (OptionsParser { decoder }) =
    decoder


failIfUnexpectedOptions : OptionsParser cliOptions builderState -> OptionsParser cliOptions builderState
failIfUnexpectedOptions ((OptionsParser ({ decoder } as optionsParser)) as fullOptionsParser) =
    OptionsParser
        { optionsParser
            | decoder =
                \flagsAndOperands ->
                    let
                        unexpectedOptions =
                            unexpectedOptions_ fullOptionsParser flagsAndOperands.options
                    in
                    if List.isEmpty unexpectedOptions then
                        decoder flagsAndOperands

                    else
                        Cli.Decode.UnexpectedOptions unexpectedOptions |> Err
        }


unexpectedOptions_ : OptionsParser cliOptions builderState -> List ParsedOption -> List String
unexpectedOptions_ (OptionsParser { usageSpecs }) options =
    List.filterMap
        (\(Tokenizer.ParsedOption optionName _) ->
            if UsageSpec.optionExists usageSpecs optionName == Nothing then
                Just optionName

            else
                Nothing
        )
        options


{-| An `OptionsParser` represents one possible way to interpret command line arguments.
A `Cli.Program.Config` can be built up using one or more `OptionsParser`s. It will
try each parser in order until one succeeds. If none succeed, it will print
an error message with information for the user of the Command-Line Interface.
-}
type OptionsParser cliOptions builderState
    = OptionsParser (OptionsParserRecord cliOptions)


{-| Low-level function, for internal use.
-}
end : OptionsParser cliOptions builderState -> OptionsParser cliOptions BuilderState.NoMoreOptions
end (OptionsParser record) =
    OptionsParser record


type alias OptionsParserRecord cliOptions =
    { decoder : Decoder cliOptions
    , usageSpecs : List UsageSpec
    , description : Maybe String
    , subCommand : Maybe String
    }


type alias Decoder cliOptions =
    { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, cliOptions )


updateDecoder : Decoder mappedCliOptions -> OptionsParser cliOptions fromBuilderState -> OptionsParser mappedCliOptions toBuilderState
updateDecoder decoder (OptionsParser optionsParserRecord) =
    OptionsParser
        { decoder = decoder
        , usageSpecs = optionsParserRecord.usageSpecs
        , description = optionsParserRecord.description
        , subCommand = optionsParserRecord.subCommand
        }


{-| Start an `OptionsParser` pipeline with no sub-command (see
[the OptionsParser terminilogy legend](https://github.com/dillonkearns/elm-cli-options-parser#options-parser-terminology)).
-}
build : cliOptions -> OptionsParser cliOptions BuilderState.AnyOptions
build cliOptionsConstructor =
    OptionsParser
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], cliOptionsConstructor )
        , subCommand = Nothing
        }


{-| Start an `OptionsParser` pipeline with a sub-command (see
[the OptionsParser terminilogy legend](https://github.com/dillonkearns/elm-cli-options-parser#options-parser-terminology)).
-}
buildSubCommand : String -> cliOptions -> OptionsParser cliOptions BuilderState.AnyOptions
buildSubCommand subCommandName cliOptionsConstructor =
    OptionsParser
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], cliOptionsConstructor )
        , subCommand = Just subCommandName
        }


{-| Use a fixed value for the next step in the pipeline. This doesn't use
any input from the user, it just passes the supplied value through in the chain.

    import Cli.Option as Option
    import Cli.OptionsParser as OptionsParser
    import Cli.Program as Program

    type alias GreetOptions =
        { name : String
        , maybeGreeting : Maybe String
        , hardcodedValue : String
        }

    programConfig : Program.Config GreetOptions
    programConfig =
        Program.config
            |> Program.add
                (OptionsParser.build GreetOptions
                    |> OptionsParser.with (Option.requiredKeywordArg "name")
                    |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                    |> OptionsParser.hardcoded "any hardcoded value"
                )

-}
hardcoded : value -> OptionsParser (value -> cliOptions) BuilderState.AnyOptions -> OptionsParser cliOptions BuilderState.AnyOptions
hardcoded hardcodedValue ((OptionsParser { decoder }) as optionsParser) =
    updateDecoder (\stuff -> resultMap (\fn -> fn hardcodedValue) (decoder stuff)) optionsParser


{-| Map the CLI options returned in the `OptionsParser` using the supplied map function.

This is very handy when you want a type alias for a record with options for a
a given `OptionsParser`, but you need all of your `OptionsParser` to map into
a single union type.

    import Cli.Option as Option
    import Cli.OptionsParser as OptionsParser
    import Cli.Program as Program
    import Ports

    type CliOptions
        = Hello HelloOptions
        | Goodbye GoodbyeOptions

    type alias HelloOptions =
        { name : String
        , maybeHello : Maybe String
        }

    type alias GoodbyeOptions =
        { name : String
        , maybeGoodbye : Maybe String
        }

    programConfig : Program.Config CliOptions
    programConfig =
        Program.config
            |> Program.add
                (OptionsParser.buildSubCommand "hello" HelloOptions
                    |> OptionsParser.with (Option.requiredKeywordArg "name")
                    |> OptionsParser.with (Option.optionalKeywordArg "greeting")
                    |> OptionsParser.map Hello
                )
            |> Program.add
                (OptionsParser.buildSubCommand "goodbye" GoodbyeOptions
                    |> OptionsParser.with (Option.requiredKeywordArg "name")
                    |> OptionsParser.with (Option.optionalKeywordArg "goodbye")
                    |> OptionsParser.map Goodbye
                )

-}
map :
    (cliOptions -> mappedCliOptions)
    -> OptionsParser cliOptions builderState
    -> OptionsParser mappedCliOptions builderState
map mapFunction ((OptionsParser { decoder }) as optionsParser) =
    updateDecoder (decoder >> Result.map (Tuple.mapSecond mapFunction)) optionsParser


{-| TODO
-}
resultMap :
    (a -> value)
    -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, a )
    -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, value )
resultMap mapFunction result =
    result
        |> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value ))


{-| The `OptionsParser` will only match if the given flag is present. Often its
best to use a subcommand in these cases.
-}
expectFlag : String -> OptionsParser cliOptions BuilderState.AnyOptions -> OptionsParser cliOptions BuilderState.AnyOptions
expectFlag flagName (OptionsParser ({ usageSpecs, decoder } as optionsParser)) =
    OptionsParser
        { optionsParser
            | usageSpecs = usageSpecs ++ [ UsageSpec.flag flagName Required ]
            , decoder =
                \({ options } as stuff) ->
                    if
                        options
                            |> List.member (Tokenizer.ParsedOption flagName Tokenizer.Flag)
                    then
                        decoder stuff

                    else
                        Cli.Decode.MatchError (Cli.Decode.MissingExpectedFlag { name = flagName })
                            |> Err
        }


{-| For chaining on any `Cli.Option.Option` besides a `restArg` or an `optionalPositionalArg`.
See the `Cli.Option` module.
-}
with : Cli.Option.Option from to { c | position : Cli.Option.BeginningOption } -> OptionsParser (to -> cliOptions) BuilderState.AnyOptions -> OptionsParser cliOptions BuilderState.AnyOptions
with =
    withCommon


withCommon : Cli.Option.Option from to optionConstraint -> OptionsParser (to -> cliOptions) startOptionsParserBuilderState -> OptionsParser cliOptions endOptionsParserBuilderState
withCommon (Internal.Option innerOption) ((OptionsParser { decoder, usageSpecs }) as fullOptionsParser) =
    updateDecoder
        (\optionsAndOperands ->
            { options = optionsAndOperands.options
            , operands = optionsAndOperands.operands
            , usageSpecs = optionsAndOperands.usageSpecs
            , operandsSoFar = UsageSpec.operandCount usageSpecs
            }
                |> innerOption.dataGrabber
                |> Result.andThen (Cli.Decode.decodeFunction innerOption.decoder)
                |> Result.andThen
                    (\( validationErrors, fromValue ) ->
                        case
                            resultMap (\fn -> fn fromValue)
                                (decoder optionsAndOperands)
                        of
                            Ok ( previousValidationErrors, thing ) ->
                                Ok ( previousValidationErrors ++ validationErrors, thing )

                            value ->
                                value
                    )
        )
        fullOptionsParser
        |> (\(OptionsParser record) ->
                OptionsParser
                    { record
                        | usageSpecs = usageSpecs ++ [ innerOption.usageSpec ]
                    }
           )


{-| For chaining on `Cli.Option.optionalPositionalArg`s.
-}
withOptionalPositionalArg : Cli.Option.Option from to { c | position : Cli.Option.OptionalPositionalArgOption } -> OptionsParser (to -> cliOptions) BuilderState.AnyOptions -> OptionsParser cliOptions BuilderState.NoBeginningOptions
withOptionalPositionalArg =
    withCommon


{-| For chaining on `Cli.Option.restArgs`.
-}
withRestArgs : Cli.Option.Option from to { c | position : Cli.Option.RestArgsOption } -> OptionsParser (to -> cliOptions) startingBuilderState -> OptionsParser cliOptions BuilderState.NoMoreOptions
withRestArgs =
    withCommon


{-| Add documentation for the optionsParser.
The output shows up after a `#` in the help output:

```bash
$ git --help
git init # initialize a git repository
...
```

      import Cli.OptionsParser as OptionsParser exposing (OptionsParser, with)

      type GitOptionsParser =
        Init
        | Clone String

      gitInitOptionsParser : OptionsParser GitOptionsParser
      gitInitOptionsParser =
        OptionsParser.build Init
         |> OptionsParser.end
         |> OptionsParser.withDoc "initialize a git repository"

-}
withDoc : String -> OptionsParser cliOptions anything -> OptionsParser cliOptions anything
withDoc docString (OptionsParser optionsParserRecord) =
    OptionsParser
        { optionsParserRecord
            | description = Just docString
        }
