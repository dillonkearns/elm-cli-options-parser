module Cli.Command
    exposing
        ( Command
        , CommandBuilder
        , build
        , buildSubCommand
        , expectFlag
        , getSubCommand
        , getUsageSpecs
        , hardcoded
        , map
        , synopsis
        , tryMatch
        , with
        , withDoc
        , withRestArgs
        , withoutRestArgs
        )

{-| TODO
@docs Command, CommandBuilder


## Start Building

@docs build, buildSubCommand


## End Building

@docs withRestArgs, withoutRestArgs


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
getUsageSpecs : Command decodesTo -> List UsageSpec
getUsageSpecs (Command { usageSpecs }) =
    usageSpecs


{-| Low-level function, for internal use.
-}
synopsis : String -> Command decodesTo -> String
synopsis programName command =
    command
        |> (\(Command record) -> record)
        |> UsageSpec.synopsis programName


{-| Low-level function, for internal use.
-}
getSubCommand : Command msg -> Maybe String
getSubCommand (Command { buildSubCommand }) =
    buildSubCommand


{-| Low-level function, for internal use.
-}
tryMatch : List String -> Command msg -> Cli.Command.MatchResult.MatchResult msg
tryMatch argv ((Command { usageSpecs, buildSubCommand }) as command) =
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


expectedPositionalArgCountOrFail : Command msg -> Command msg
expectedPositionalArgCountOrFail (Command ({ decoder, usageSpecs } as command)) =
    Command
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
getDecoder (Command { decoder }) =
    decoder


failIfUnexpectedOptions : Command msg -> Command msg
failIfUnexpectedOptions ((Command ({ decoder, usageSpecs } as command)) as fullCommand) =
    Command
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


unexpectedOptions_ : Command msg -> List ParsedOption -> List String
unexpectedOptions_ (Command { usageSpecs }) options =
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
type CommandBuilder msg
    = CommandBuilder (CommandRecord msg)


{-| Turn a `CommandBuilder` into a `Command` which can be used with `Cli.OptionsParser.run`.
The command will fail if any unspecific positional arguments are passed in.

    type GitCommand
        = Init
        | Clone

    initCommand =
        Command.buildSubCommand "init" Init
            |> Command.withoutRestArgs


    {-
       $ git init
       # matches Init
       $ git init positionalArg
       # doesn't match init command
    -}

-}
withoutRestArgs : CommandBuilder msg -> Command msg
withoutRestArgs (CommandBuilder record) =
    Command record


{-| Turn a `CommandBuilder` into a `Command` which can be used with `Cli.OptionsParser.run`.
The command will succeed if any unspecific positional arguments are passed in and will capture them in a list.

    type GitCommand
        = Init
        | Add (List String)

    addCommand =
        Command.buildSubCommand "add" Add
            |> Command.withRestArgs


    {-
       $ git add
       # matches Add []
       $ git add new-file1.txt new-file2.txt
       # matches Add ["new-file1.txt", "new-file2.txt"]
    -}

If you need at least one positional argument, then just use `Cli.Option.positionalArg`.

-}
withRestArgs : String -> CommandBuilder (List String -> msg) -> Command msg
withRestArgs restOperandsDescription (CommandBuilder ({ usageSpecs, description, decoder } as record)) =
    Command
        { usageSpecs = usageSpecs ++ [ UsageSpec.restArgs restOperandsDescription ]
        , description = description
        , decoder =
            \({ operands } as stuff) ->
                let
                    restOperands =
                        operands
                            |> List.drop (UsageSpec.operandCount usageSpecs)
                in
                resultMap (\fn -> fn restOperands) (decoder stuff)
        , buildSubCommand = record.buildSubCommand
        }


type alias CommandRecord msg =
    { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
    , usageSpecs : List UsageSpec
    , description : Maybe String
    , buildSubCommand : Maybe String
    }


{-| TODO
-}
type Command msg
    = Command (CommandRecord msg)


{-| TODO
-}
build : msg -> CommandBuilder msg
build msgConstructor =
    CommandBuilder
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        , buildSubCommand = Nothing
        }


{-| TODO
-}
buildSubCommand : String -> msg -> CommandBuilder msg
buildSubCommand buildSubCommandName msgConstructor =
    CommandBuilder
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        , buildSubCommand = Just buildSubCommandName
        }


{-| TODO
-}
hardcoded : value -> CommandBuilder (value -> msg) -> CommandBuilder msg
hardcoded hardcodedValue (CommandBuilder ({ decoder } as command)) =
    CommandBuilder
        { command
            | decoder =
                \stuff -> resultMap (\fn -> fn hardcodedValue) (decoder stuff)
        }


{-| TODO
-}
map : (msg -> mappedMsg) -> Command msg -> Command mappedMsg
map mapFunction (Command ({ decoder } as record)) =
    Command { record | decoder = decoder >> Result.map (Tuple.mapSecond mapFunction) }


{-| TODO
-}
resultMap : (a -> value) -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, a ) -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, value )
resultMap mapFunction result =
    result
        |> Result.map (\( validationErrors, value ) -> ( validationErrors, mapFunction value ))


{-| TODO
-}
expectFlag : String -> CommandBuilder msg -> CommandBuilder msg
expectFlag flagName (CommandBuilder ({ usageSpecs, decoder } as command)) =
    CommandBuilder
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
            |> Command.withoutRestArgs
            |> Command.map Log

        -- ...
        ]

-}
with : Option from to -> CommandBuilder (to -> msg) -> CommandBuilder msg
with (Option dataGrabber usageSpec optionsDecoder) ((CommandBuilder ({ decoder, usageSpecs } as command)) as fullCommand) =
    CommandBuilder
        { command
            | decoder =
                \optionsAndOperands ->
                    { options = optionsAndOperands.options
                    , operands = optionsAndOperands.operands
                    , usageSpecs = optionsAndOperands.usageSpecs
                    , operandsSoFar = UsageSpec.operandCount usageSpecs
                    }
                        |> dataGrabber
                        |> Result.andThen (Cli.Decode.decodeFunction optionsDecoder)
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
            , usageSpecs = usageSpecs ++ [ usageSpec ]
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
         |> Command.withoutRestArgs
         |> Command.withDoc "initialize a git repository"

-}
withDoc : String -> Command msg -> Command msg
withDoc docString (Command commandRecord) =
    Command
        { commandRecord
            | description = Just docString
        }
