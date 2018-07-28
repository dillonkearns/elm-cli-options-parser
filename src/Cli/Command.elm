module Cli.Command
    exposing
        ( ActualCommand
        , Command
        , CommandBuilder
        , CompletedBuilder
        , InProgressBuilder
        , build
        , buildSubCommand
        , end
        , endWith
        , expectFlag
        , getSubCommand
        , getUsageSpecs
        , hardcoded
        , map
        , synopsis
        , tryMatch
        , with
        , withDoc
        )

{-|


## Types

You start building with a `CommandBuilder`. At the end,
turn your `CommandBuilder` into a `Command` by calling
`Command.end` or `Command.endWith`.

@docs Command, CommandBuilder, ActualCommand, CompletedBuilder, InProgressBuilder


## Start Building

@docs build, buildSubCommand


## End Building

@docs end

The new way:

@docs endWith


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

import Cli.Command.MatchResult
import Cli.Decode
import Cli.Option exposing (Option(Option))
import Cli.UsageSpec as UsageSpec exposing (UsageSpec)
import Occurences exposing (Occurences(..))
import Tokenizer exposing (ParsedOption)


{-| TODO
-}
getUsageSpecs : ActualCommand decodesTo builderStatus -> List UsageSpec
getUsageSpecs (ActualCommand { usageSpecs }) =
    usageSpecs


{-| Low-level function, for internal use.
-}
synopsis : String -> ActualCommand decodesTo builderStatus -> String
synopsis programName command =
    command
        |> (\(ActualCommand record) -> record)
        |> UsageSpec.synopsis programName


{-| Low-level function, for internal use.
-}
getSubCommand : ActualCommand msg builderStatus -> Maybe String
getSubCommand (ActualCommand { buildSubCommand }) =
    buildSubCommand


{-| Low-level function, for internal use.
-}
tryMatch : List String -> ActualCommand msg builderStatus -> Cli.Command.MatchResult.MatchResult msg
tryMatch argv ((ActualCommand { usageSpecs, buildSubCommand }) as command) =
    let
        decoder =
            command
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
                                    Err { errorMessage = "Sub command does not match", options = record.options }

                            ( Just buildSubCommandName, [] ) ->
                                Err { errorMessage = "No sub command provided", options = record.options }
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
                                        Cli.Command.MatchResult.NoMatch []

                                    Cli.Decode.UnrecoverableValidationError validationError ->
                                        Cli.Command.MatchResult.Match (Err [ validationError ])

                                    Cli.Decode.UnexpectedOptions unexpectedOptions ->
                                        Cli.Command.MatchResult.NoMatch unexpectedOptions

                            Ok ( [], value ) ->
                                Cli.Command.MatchResult.Match (Ok value)

                            Ok ( validationErrors, value ) ->
                                Cli.Command.MatchResult.Match (Err validationErrors)
                   )

        Err { errorMessage, options } ->
            Cli.Command.MatchResult.NoMatch (unexpectedOptions_ command options)


expectedPositionalArgCountOrFail : ActualCommand msg builderStatus -> Command msg
expectedPositionalArgCountOrFail (ActualCommand ({ decoder, usageSpecs } as command)) =
    ActualCommand
        { command
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
    Command msg
    ->
        { operands : List String
        , options : List ParsedOption
        , usageSpecs : List UsageSpec
        }
    -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
getDecoder (ActualCommand { decoder }) =
    decoder


failIfUnexpectedOptions : Command msg -> Command msg
failIfUnexpectedOptions ((ActualCommand ({ decoder, usageSpecs } as command)) as fullCommand) =
    ActualCommand
        { command
            | decoder =
                \flagsAndOperands ->
                    let
                        unexpectedOptions =
                            unexpectedOptions_ fullCommand flagsAndOperands.options
                    in
                    if List.isEmpty unexpectedOptions then
                        decoder flagsAndOperands
                    else
                        Cli.Decode.UnexpectedOptions unexpectedOptions |> Err
        }


unexpectedOptions_ : ActualCommand msg builderStatus -> List ParsedOption -> List String
unexpectedOptions_ (ActualCommand { usageSpecs }) options =
    List.filterMap
        (\(Tokenizer.ParsedOption optionName optionKind) ->
            if UsageSpec.optionExists usageSpecs optionName == Nothing then
                Just optionName
            else
                Nothing
        )
        options


{-| -}
type InProgressBuilder
    = InProgressBuilder


{-| -}
type CompletedBuilder
    = CompletedBuilder


{-| TODO
-}
type ActualCommand msg builderStatus
    = ActualCommand (CommandRecord msg)


{-| Turn a `CommandBuilder` into a `Command` which can be used with `Cli.OptionsParser.run`.
The command will fail if any unspecific positional arguments are passed in.

    type GitCommand
        = Init
        | Clone

    initCommand =
        Command.buildSubCommand "init" Init
            |> Command.end


    {-
       $ git init
       # matches Init
       $ git init positionalArg
       # doesn't match init command
    -}

-}
end : ActualCommand msg anything -> Command msg
end (ActualCommand record) =
    ActualCommand record


type alias CommandRecord msg =
    { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
    , usageSpecs : List UsageSpec
    , description : Maybe String
    , buildSubCommand : Maybe String
    }


{-| TODO
-}
type alias Command msg =
    ActualCommand msg CompletedBuilder


{-| TODO
-}
type alias CommandBuilder msg =
    ActualCommand msg InProgressBuilder


{-| TODO
-}
build : msg -> CommandBuilder msg
build msgConstructor =
    ActualCommand
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        , buildSubCommand = Nothing
        }


{-| TODO
-}
buildSubCommand : String -> msg -> CommandBuilder msg
buildSubCommand buildSubCommandName msgConstructor =
    ActualCommand
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        , buildSubCommand = Just buildSubCommandName
        }


{-| TODO
-}
hardcoded : value -> CommandBuilder (value -> msg) -> CommandBuilder msg
hardcoded hardcodedValue (ActualCommand ({ decoder } as command)) =
    ActualCommand
        { command
            | decoder =
                \stuff -> resultMap (\fn -> fn hardcodedValue) (decoder stuff)
        }


{-| TODO
-}
map : (msg -> mappedMsg) -> Command msg -> Command mappedMsg
map mapFunction (ActualCommand ({ decoder } as record)) =
    ActualCommand { record | decoder = decoder >> Result.map (Tuple.mapSecond mapFunction) }


{-| TODO
-}
resultMap : (a -> value) -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, a ) -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, value )
resultMap mapFunction result =
    result
        |> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value ))


{-| TODO
-}
expectFlag : String -> CommandBuilder msg -> CommandBuilder msg
expectFlag flagName (ActualCommand ({ usageSpecs, decoder } as command)) =
    ActualCommand
        { command
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


{-| Include an `Option` in your `Command`, see the `Cli.Option` module.

    import Cli.Command as Command exposing (Command, with)
    import Cli.Option

    type GitCommand
        = Init
        | Log LogOptions -- ...

    type alias LogOptions =
        { maybeAuthorPattern : Maybe String
        , maybeNumberToDisplay : Maybe Int
        }

    commands : List (Command GitCommand)
    commands =
        [ Command.buildSubCommand "log" LogOptions
            |> with
                (Cli.Option.optionalKeywordArg "author")
            |> with
                (Cli.Option.optionalKeywordArg "number"
                    |> Cli.Option.validateMapIfPresent String.toInt
                )
            |> Command.end
            |> Command.map Log

        -- ...
        ]

-}
with : Option from to Cli.Option.MiddleOption -> CommandBuilder (to -> msg) -> CommandBuilder msg
with (Option innerOption) ((ActualCommand ({ decoder, usageSpecs } as command)) as fullCommand) =
    ActualCommand
        { command
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


{-| Turn a `CommandBuilder` into a `Command` which can be used with `Cli.OptionsParser.run`.
The command will succeed if any unspecific positional arguments are passed in and will capture them in a list.

    type GitCommand
        = Init
        | Add (List String)

    addCommand =
        Command.buildSubCommand "add" Add
            |> Command.endWith (Option.restArgs "files")


    {-
       $ git add
       # matches Add []
       $ git add new-file1.txt new-file2.txt
       # matches Add ["new-file1.txt", "new-file2.txt"]
    -}

If you need at least one positional argument, then just use `Cli.Option.positionalArg`.

-}
endWith : Option from to Cli.Option.EndingOption -> CommandBuilder (to -> msg) -> Command msg
endWith (Option innerOption) ((ActualCommand ({ decoder, usageSpecs } as command)) as fullCommand) =
    ActualCommand
        { command
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


{-| Add documentation for the command.
The output shows up after a `#` in the help output:

```bash
$ git --help
git init # initialize a git repository
...
```

      import Cli.Command as Command exposing (Command, with)

      type GitCommand =
        Init
        | Clone String

      gitInitCommand : Command GitCommand
      gitInitCommand =
        Command.build Init
         |> Command.end
         |> Command.withDoc "initialize a git repository"

-}
withDoc : String -> ActualCommand msg anything -> ActualCommand msg anything
withDoc docString (ActualCommand commandRecord) =
    ActualCommand
        { commandRecord
            | description = Just docString
        }
