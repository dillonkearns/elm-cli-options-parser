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
Elm gives us a `GreetOptions` which looks like `String -> Maybe String -> GreetOptions`
(this is just a core Elm language feature). That is, if we pass in a `String` and
a `Maybe String` as the 1st and 2nd arguments, we get a `GreetOptions` record.

So in this example, we call `OptionsParser.build` with our `GreetOptions`
constructor function. Then we call `with` once for each of those two arguments.
Note that the first with will give us a `String`, and the second will give us
a `Maybe String`, so it matches up perfectly with the order of our constructor's
arguments.

    import Cli.Option as Option
    import Cli.OptionsParser as OptionsParser exposing (with)
    import Cli.Program as Program

    type alias GreetOptions =
        { name : String
        , maybeGreeting : Maybe String
        }

    program : Program.Config GreetOptions
    program =
        Program.config { version = "1.2.3" }
            |> Program.add
                (OptionsParser.build GreetOptions
                    |> with (Option.requiredKeywordArg "name")
                    |> with (Option.optionalKeywordArg "greeting")
                )

@docs build, buildSubCommand


## Adding `Cli.Option.Option`s To The Pipeline

Start the chain using `with`:
@docs with
If you need to add `restArgs` or `optionalPositionalArg`s, they must be added
in the correct order. So instead of using `with`, you use the corresponding
`with...` function:
@docs withOptionalPositionalArg, withRestArgs


## I wish this could be in Option...

@docs expectFlag


## Mapping and Transforming

@docs map
@docs hardcoded


## Meta-Data

@docs withDoc


## Low-Level, can I get rid of these?

@docs getSubCommand, getUsageSpecs, synopsis, tryMatch, end

-}

import Cli.Decode
import Cli.Option exposing (Option(Option))
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.OptionsParser.MatchResult
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Occurences exposing (Occurences(..))
import Tokenizer exposing (ParsedOption)


{-| TODO
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
getSubCommand (OptionsParser { buildSubCommand }) =
    buildSubCommand


{-| Low-level function, for internal use.
-}
tryMatch : List String -> OptionsParser msg builderState -> Cli.OptionsParser.MatchResult.MatchResult msg
tryMatch argv ((OptionsParser { usageSpecs, buildSubCommand }) as optionsParser) =
    let
        decoder =
            optionsParser
                |> expectedPositionalArgCountOrFail
                |> failIfUnexpectedOptions
                |> getDecoder

        flagsAndOperands =
            Tokenizer.flagsAndOperands usageSpecs argv
                |> (\record ->
                        case ( buildSubCommand, record.operands ) of
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


{-| Turn a `OptionsParserBuilder` into a `OptionsParser` which can be used with `Cli.Program.run`.
The optionsParser will fail if any unspecific positional arguments are passed in.

    type GitOptionsParser
        = Init
        | Clone

    initOptionsParser =
        OptionsParser.buildSubCommand "init" Init
            |> OptionsParser.end


    {-
       $ git init
       # matches Init
       $ git init positionalArg
       # doesn't match init optionsParser
    -}

-}
end : OptionsParser msg anything -> OptionsParser msg BuilderState.NoMoreOptions
end (OptionsParser record) =
    OptionsParser record


type alias OptionsParserRecord msg =
    { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
    , usageSpecs : List UsageSpec
    , description : Maybe String
    , buildSubCommand : Maybe String
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
        , buildSubCommand = Nothing
        }


{-| Start an `OptionsParser` pipeline with a sub-command (see
[the OptionsParser terminilogy legend](https://github.com/dillonkearns/elm-cli-options-parser#options-parser-terminology)).
-}
buildSubCommand : String -> msg -> OptionsParser msg BuilderState.AnyOptions
buildSubCommand buildSubCommandName cliOptionsConstructor =
    OptionsParser
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], cliOptionsConstructor )
        , buildSubCommand = Just buildSubCommandName
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


{-| TODO
-}
map : (msg -> mappedMsg) -> OptionsParser msg builderState -> OptionsParser mappedMsg builderState
map mapFunction (OptionsParser ({ decoder } as record)) =
    OptionsParser { record | decoder = decoder >> Result.map (Tuple.mapSecond mapFunction) }


{-| TODO
-}
resultMap : (a -> value) -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, a ) -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, value )
resultMap mapFunction result =
    result
        |> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value ))


{-| TODO
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


{-| Include an `Option` in your `OptionsParser`, see the `Cli.Option` module.

    import Cli.Option
    import Cli.OptionsParser as OptionsParser exposing (OptionsParser, with)

    type GitOptionsParser
        = Init
        | Log LogOptions -- ...

    type alias LogOptions =
        { maybeAuthorPattern : Maybe String
        , maybeNumberToDisplay : Maybe Int
        }

    optionsParsers : List (OptionsParser GitOptionsParser)
    optionsParsers =
        [ OptionsParser.buildSubCommand "log" LogOptions
            |> with
                (Cli.Option.optionalKeywordArg "author")
            |> with
                (Cli.Option.optionalKeywordArg "number"
                    |> Cli.Option.validateMapIfPresent String.toInt
                )
            |> OptionsParser.end
            |> OptionsParser.map Log

        -- ...
        ]

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


{-| Turn a `OptionsParserBuilder` into a `OptionsParser` which can be used with `Cli.Program.run`.
The optionsParser will succeed if any unspecific positional arguments are passed in and will capture them in a list.

    type GitOptionsParser
        = Init
        | Add (List String)

    addOptionsParser =
        OptionsParser.buildSubCommand "add" Add
            |> OptionsParser.withRestArgs (Option.restArgs "files")


    {-
       $ git add
       # matches Add []
       $ git add new-file1.txt new-file2.txt
       # matches Add ["new-file1.txt", "new-file2.txt"]
    -}

If you need at least one positional argument, then just use `Cli.Option.positionalArg`.

-}
withOptionalPositionalArg : Option from to Cli.Option.MiddleOption -> OptionsParser (to -> msg) BuilderState.AnyOptions -> OptionsParser msg BuilderState.NoBeginningOptions
withOptionalPositionalArg =
    withCommon


{-| For chaining on `Cli.Option.restArgs`.
-}
withRestArgs : Option from to Cli.Option.TerminalOption -> OptionsParser (to -> msg) startingBuilderState -> OptionsParser msg BuilderState.NoMoreOptions
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
