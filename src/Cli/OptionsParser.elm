module Cli.OptionsParser
    exposing
        ( ActualOptionsParser
        , OptionsParser
        , OptionsParserBuilder
        , TerminalOptionsParser
        , build
        , buildSubCommand
        , end
        , expectFlag
        , getSubCommand
        , getUsageSpecs
        , hardcoded
        , map
        , optionalPositionalArg
        , restArgs
        , synopsis
        , tryMatch
        , with
        , withDoc
        )

{-|


## Types

You start building with a `OptionsParserBuilder`. At the end,
turn your `OptionsParserBuilder` into a `OptionsParser` by calling
`OptionsParser.end` or `OptionsParser.optionalPositionalArg`.

@docs OptionsParser, OptionsParserBuilder, ActualOptionsParser, TerminalOptionsParser


## Start Building

@docs build, buildSubCommand


## End Building

@docs end

The new way:

@docs optionalPositionalArg, restArgs


## Middle

Start the chain using `with`:
@docs with


## I wish this could be in Option...

@docs expectFlag


## Other Stuff

@docs hardcoded, withDoc


## Mapping

@docs map


## Low-Level, can I get rid of these?

@docs getSubCommand, getUsageSpecs, synopsis, tryMatch

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
getUsageSpecs : ActualOptionsParser decodesTo builderStatus -> List UsageSpec
getUsageSpecs (ActualOptionsParser { usageSpecs }) =
    usageSpecs


{-| Low-level function, for internal use.
-}
synopsis : String -> ActualOptionsParser decodesTo builderStatus -> String
synopsis programName optionsParser =
    optionsParser
        |> (\(ActualOptionsParser record) -> record)
        |> UsageSpec.synopsis programName


{-| Low-level function, for internal use.
-}
getSubCommand : ActualOptionsParser msg builderStatus -> Maybe String
getSubCommand (ActualOptionsParser { buildSubCommand }) =
    buildSubCommand


{-| Low-level function, for internal use.
-}
tryMatch : List String -> ActualOptionsParser msg builderStatus -> Cli.OptionsParser.MatchResult.MatchResult msg
tryMatch argv ((ActualOptionsParser { usageSpecs, buildSubCommand }) as optionsParser) =
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


expectedPositionalArgCountOrFail : ActualOptionsParser msg builderStatus -> OptionsParser msg
expectedPositionalArgCountOrFail (ActualOptionsParser ({ decoder, usageSpecs } as optionsParser)) =
    ActualOptionsParser
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
    OptionsParser msg
    ->
        { operands : List String
        , options : List ParsedOption
        , usageSpecs : List UsageSpec
        }
    -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
getDecoder (ActualOptionsParser { decoder }) =
    decoder


failIfUnexpectedOptions : OptionsParser msg -> OptionsParser msg
failIfUnexpectedOptions ((ActualOptionsParser ({ decoder, usageSpecs } as optionsParser)) as fullOptionsParser) =
    ActualOptionsParser
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


unexpectedOptions_ : ActualOptionsParser msg builderStatus -> List ParsedOption -> List String
unexpectedOptions_ (ActualOptionsParser { usageSpecs }) options =
    List.filterMap
        (\(Tokenizer.ParsedOption optionName optionKind) ->
            if UsageSpec.optionExists usageSpecs optionName == Nothing then
                Just optionName
            else
                Nothing
        )
        options


{-| TODO
-}
type ActualOptionsParser msg builderStatus
    = ActualOptionsParser (OptionsParserRecord msg)


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
end : ActualOptionsParser msg anything -> TerminalOptionsParser msg
end (ActualOptionsParser record) =
    ActualOptionsParser record


type alias OptionsParserRecord msg =
    { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
    , usageSpecs : List UsageSpec
    , description : Maybe String
    , buildSubCommand : Maybe String
    }


{-| TODO
-}
type alias OptionsParser msg =
    ActualOptionsParser msg BuilderState.EndOptionsOnly


{-| TODO
-}
type alias OptionsParserBuilder msg =
    ActualOptionsParser msg BuilderState.AnyOptions


{-| TODO
-}
type alias TerminalOptionsParser msg =
    ActualOptionsParser msg BuilderState.Terminal


{-| TODO
-}
build : msg -> OptionsParserBuilder msg
build msgConstructor =
    ActualOptionsParser
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        , buildSubCommand = Nothing
        }


{-| TODO
-}
buildSubCommand : String -> msg -> OptionsParserBuilder msg
buildSubCommand buildSubCommandName msgConstructor =
    ActualOptionsParser
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        , buildSubCommand = Just buildSubCommandName
        }


{-| TODO
-}
hardcoded : value -> OptionsParserBuilder (value -> msg) -> OptionsParserBuilder msg
hardcoded hardcodedValue (ActualOptionsParser ({ decoder } as optionsParser)) =
    ActualOptionsParser
        { optionsParser
            | decoder =
                \stuff -> resultMap (\fn -> fn hardcodedValue) (decoder stuff)
        }


{-| TODO
-}
map : (msg -> mappedMsg) -> ActualOptionsParser msg builderState -> ActualOptionsParser mappedMsg builderState
map mapFunction (ActualOptionsParser ({ decoder } as record)) =
    ActualOptionsParser { record | decoder = decoder >> Result.map (Tuple.mapSecond mapFunction) }


{-| TODO
-}
resultMap : (a -> value) -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, a ) -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, value )
resultMap mapFunction result =
    result
        |> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value ))


{-| TODO
-}
expectFlag : String -> OptionsParserBuilder msg -> OptionsParserBuilder msg
expectFlag flagName (ActualOptionsParser ({ usageSpecs, decoder } as optionsParser)) =
    ActualOptionsParser
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
with : Option from to Cli.Option.BeginningOption -> OptionsParserBuilder (to -> msg) -> OptionsParserBuilder msg
with =
    withCommon


withCommon : Option from to optionConstraint -> ActualOptionsParser (to -> msg) startOptionsParserBuilderState -> ActualOptionsParser msg endOptionsParserBuilderState
withCommon (Option innerOption) ((ActualOptionsParser ({ decoder, usageSpecs } as optionsParser)) as fullOptionsParser) =
    ActualOptionsParser
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
            |> OptionsParser.optionalPositionalArg (Option.restArgs "files")


    {-
       $ git add
       # matches Add []
       $ git add new-file1.txt new-file2.txt
       # matches Add ["new-file1.txt", "new-file2.txt"]
    -}

If you need at least one positional argument, then just use `Cli.Option.positionalArg`.

-}
optionalPositionalArg : Option from to Cli.Option.MiddleOption -> OptionsParserBuilder (to -> msg) -> OptionsParser msg
optionalPositionalArg =
    withCommon


{-| For chaining on `Cli.Option.restArgs`.
-}
restArgs : Option from to Cli.Option.TerminalOption -> ActualOptionsParser (to -> msg) startingBuilderState -> TerminalOptionsParser msg
restArgs =
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
withDoc : String -> ActualOptionsParser msg anything -> ActualOptionsParser msg anything
withDoc docString (ActualOptionsParser optionsParserRecord) =
    ActualOptionsParser
        { optionsParserRecord
            | description = Just docString
        }
