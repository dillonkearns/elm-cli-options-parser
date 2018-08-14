module Cli.OptionsParser
    exposing
        ( OptionsParser
        , build
        , buildSubCommand
        , end
        , expectFlag
        , getSubCommand
        , getUsageSpecs
        , hardcoded
        , map
        , synopsis
        , tryMatch
        , with
        , withDoc
        , withOptionalPositionalArg
        , withRestArgs
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
        Program.config { version = "1.2.3" }
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

@docs getSubCommand, getUsageSpecs, synopsis, tryMatch, end

-}

import Cli.Decode
import Cli.Option exposing (Option(Option))
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
-}
getSubCommand : OptionsParser msg builderState -> Maybe String
getSubCommand (OptionsParser { subCommand }) =
    subCommand


{-| Low-level function, for internal use.
-}
tryMatch : List String -> OptionsParser msg builderState -> Cli.OptionsParser.MatchResult.MatchResult msg
tryMatch argv ((OptionsParser { usageSpecs, subCommand }) as optionsParser) =
    let
        decoder =
            optionsParser
                |> expectedPositionalArgCountOrFail
                |> failIfUnexpectedOptions
                |> getDecoder

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

                            ( Just buildSubCommandName, actualSubCommand :: remainingOperands ) ->
                                if actualSubCommand == buildSubCommandName then
                                    Ok
                                        { options = record.options
                                        , operands = remainingOperands
                                        , usageSpecs = usageSpecs
                                        }
                                else
                                    Err { errorMessage = "Sub optionsParser does not match", options = record.options }

                            ( Just buildSubCommandName, [] ) ->
                                Err { errorMessage = "No sub optionsParser provided", options = record.options }
                   )
    in
    case flagsAndOperands of
        Ok actualFlagsAndOperands ->
            decoder actualFlagsAndOperands
                |> (\result ->
                        case result of
                            Err error ->
                                case error of
                                    Cli.Decode.MatchError matchError ->
                                        Cli.OptionsParser.MatchResult.NoMatch []

                                    Cli.Decode.UnrecoverableValidationError validationError ->
                                        Cli.OptionsParser.MatchResult.Match (Err [ validationError ])

                                    Cli.Decode.UnexpectedOptions unexpectedOptions ->
                                        Cli.OptionsParser.MatchResult.NoMatch unexpectedOptions

                            Ok ( [], value ) ->
                                Cli.OptionsParser.MatchResult.Match (Ok value)

                            Ok ( validationErrors, value ) ->
                                Cli.OptionsParser.MatchResult.Match (Err validationErrors)
                   )

        Err { errorMessage, options } ->
            Cli.OptionsParser.MatchResult.NoMatch (unexpectedOptions_ optionsParser options)


expectedPositionalArgCountOrFail : OptionsParser msg builderState -> OptionsParser msg builderState
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
                        Cli.Decode.MatchError "Wrong number of operands" |> Err
                    else
                        decoder stuff
        }


getDecoder :
    OptionsParser msg builderState
    ->
        { operands : List String
        , options : List ParsedOption
        , usageSpecs : List UsageSpec
        }
    -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
getDecoder (OptionsParser { decoder }) =
    decoder


failIfUnexpectedOptions : OptionsParser msg builderState -> OptionsParser msg builderState
failIfUnexpectedOptions ((OptionsParser ({ decoder, usageSpecs } as optionsParser)) as fullOptionsParser) =
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


unexpectedOptions_ : OptionsParser msg builderState -> List ParsedOption -> List String
unexpectedOptions_ (OptionsParser { usageSpecs }) options =
    List.filterMap
        (\(Tokenizer.ParsedOption optionName optionKind) ->
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
type OptionsParser msg builderState
    = OptionsParser (OptionsParserRecord msg)


{-| Low-level function, for internal use.
-}
end : OptionsParser msg anything -> OptionsParser msg BuilderState.NoMoreOptions
end (OptionsParser record) =
    OptionsParser record


type alias OptionsParserRecord msg =
    { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
    , usageSpecs : List UsageSpec
    , description : Maybe String
    , subCommand : Maybe String
    }


{-| Start an `OptionsParser` pipeline with no sub-command (see
[the OptionsParser terminilogy legend](https://github.com/dillonkearns/elm-cli-options-parser#options-parser-terminology)).
-}
build : msg -> OptionsParser msg BuilderState.AnyOptions
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
buildSubCommand : String -> msg -> OptionsParser msg BuilderState.AnyOptions
buildSubCommand subCommandName cliOptionsConstructor =
    OptionsParser
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], cliOptionsConstructor )
        , subCommand = Just subCommandName
        }


{-| TODO
-}
hardcoded : value -> OptionsParser (value -> msg) BuilderState.AnyOptions -> OptionsParser msg BuilderState.AnyOptions
hardcoded hardcodedValue (OptionsParser ({ decoder } as optionsParser)) =
    OptionsParser
        { optionsParser
            | decoder =
                \stuff -> resultMap (\fn -> fn hardcodedValue) (decoder stuff)
        }


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
        Program.config { version = "1.2.3" }
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
map mapFunction (OptionsParser ({ decoder } as record)) =
    OptionsParser { record | decoder = decoder >> Result.map (Tuple.mapSecond mapFunction) }


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
expectFlag : String -> OptionsParser msg BuilderState.AnyOptions -> OptionsParser msg BuilderState.AnyOptions
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
                        Cli.Decode.MatchError ("Expect flag " ++ ("--" ++ flagName))
                            |> Err
        }


{-| For chaining on any `Cli.Option.Option` besides a `restArg` or an `optionalPositionalArg`.
See the `Cli.Option` module.
-}
with : Option from to Cli.Option.BeginningOption -> OptionsParser (to -> msg) BuilderState.AnyOptions -> OptionsParser msg BuilderState.AnyOptions
with =
    withCommon


withCommon : Option from to optionConstraint -> OptionsParser (to -> msg) startOptionsParserBuilderState -> OptionsParser msg endOptionsParserBuilderState
withCommon (Option innerOption) ((OptionsParser ({ decoder, usageSpecs } as optionsParser)) as fullOptionsParser) =
    OptionsParser
        { optionsParser
            | decoder =
                \optionsAndOperands ->
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
            , usageSpecs = usageSpecs ++ [ innerOption.usageSpec ]
        }


{-| For chaining on `Cli.Option.optionalPositionalArg`s.
-}
withOptionalPositionalArg : Option from to Cli.Option.OptionalPositionalArgOption -> OptionsParser (to -> msg) BuilderState.AnyOptions -> OptionsParser msg BuilderState.NoBeginningOptions
withOptionalPositionalArg =
    withCommon


{-| For chaining on `Cli.Option.restArgs`.
-}
withRestArgs : Option from to Cli.Option.RestArgsOption -> OptionsParser (to -> msg) startingBuilderState -> OptionsParser msg BuilderState.NoMoreOptions
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
withDoc : String -> OptionsParser msg anything -> OptionsParser msg anything
withDoc docString (OptionsParser optionsParserRecord) =
    OptionsParser
        { optionsParserRecord
            | description = Just docString
        }
