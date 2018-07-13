module Cli.Command
    exposing
        ( Command
        , CommandBuilder
        , build
        , buildWithDoc
        , expectFlag
        , getSubCommand
        , getUsageSpecs
        , hardcoded
        , map
        , subCommand
        , synopsis
        , tryMatch
        , tryMatchNew
        , with
        , withDefault
        , withRestArgs
        , withoutRestArgs
        )

{-| TODO
@docs Command

@docs build, buildWithDoc, withRestArgs, expectFlag, getSubCommand, getUsageSpecs, hardcoded, subCommand, synopsis, withoutRestArgs, tryMatch, tryMatchNew, with, withDefault, map

Low-level???
@docs CommandBuilder

-}

import Cli.Command.MatchResult as MatchResult exposing (MatchResult)
import Cli.Decode
import Cli.Spec exposing (CliSpec(..))
import Cli.UsageSpec as UsageSpec exposing (..)
import Cli.Validate exposing (ValidationResult(Invalid, Valid))
import Occurences exposing (Occurences(..))
import Tokenizer exposing (ParsedOption)


{-| TODO
-}
getUsageSpecs : Command decodesTo -> List UsageSpec
getUsageSpecs (Command { usageSpecs }) =
    usageSpecs


{-| TODO
-}
synopsis : String -> Command decodesTo -> String
synopsis programName command =
    command
        |> (\(Command record) -> record)
        |> UsageSpec.synopsis programName


{-| TODO
-}
getSubCommand : Command msg -> Maybe String
getSubCommand (Command { subCommand }) =
    subCommand


{-| TODO
-}
tryMatchNew : List String -> Command msg -> MatchResult msg
tryMatchNew argv ((Command { decoder, usageSpecs, subCommand }) as command) =
    let
        decoder =
            command
                |> expectedOperandCountOrFail
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

                            ( Just subCommandName, actualSubCommand :: remainingOperands ) ->
                                if actualSubCommand == subCommandName then
                                    Ok
                                        { options = record.options
                                        , operands = remainingOperands
                                        , usageSpecs = usageSpecs
                                        }
                                else
                                    Err { errorMessage = "Sub command does not match", options = record.options }

                            ( Just subCommandName, [] ) ->
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
                                        MatchResult.NoMatch []

                                    Cli.Decode.UnrecoverableValidationError validationError ->
                                        MatchResult.Match (Err [ validationError ])

                                    Cli.Decode.UnexpectedOptions unexpectedOptions ->
                                        MatchResult.NoMatch unexpectedOptions

                            Ok ( [], value ) ->
                                MatchResult.Match (Ok value)

                            Ok ( validationErrors, value ) ->
                                MatchResult.Match (Err validationErrors)
                   )

        Err { errorMessage, options } ->
            MatchResult.NoMatch (unexpectedOptions_ command options)


{-| TODO
-}
tryMatch : List String -> Command msg -> Maybe (Result (List Cli.Decode.ValidationError) msg)
tryMatch argv ((Command { decoder, usageSpecs, subCommand }) as command) =
    let
        decoder =
            command
                |> expectedOperandCountOrFail
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

                            ( Just subCommandName, actualSubCommand :: remainingOperands ) ->
                                if actualSubCommand == subCommandName then
                                    Ok
                                        { options = record.options
                                        , operands = remainingOperands
                                        , usageSpecs = usageSpecs
                                        }
                                else
                                    Err "Sub command does not match"

                            ( Just subCommandName, [] ) ->
                                Err "No sub command provided"
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
                                        Nothing

                                    Cli.Decode.UnrecoverableValidationError validationError ->
                                        Just (Err [ validationError ])

                                    Cli.Decode.UnexpectedOptions _ ->
                                        Nothing

                            Ok ( [], value ) ->
                                Just (Ok value)

                            Ok ( validationErrors, value ) ->
                                Just (Err validationErrors)
                   )

        Err _ ->
            Nothing


expectedOperandCountOrFail : Command msg -> Command msg
expectedOperandCountOrFail ((Command ({ decoder, usageSpecs } as command)) as fullCommand) =
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
unexpectedOptions_ (Command ({ decoder, usageSpecs } as command)) options =
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
    = CommandBuilder
        { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
        , usageSpecs : List UsageSpec
        , description : Maybe String
        , subCommand : Maybe String
        }


{-| TODO
-}
withoutRestArgs : CommandBuilder msg -> Command msg
withoutRestArgs (CommandBuilder record) =
    Command record


{-| TODO
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
        , subCommand = record.subCommand
        }


{-| TODO
-}
type Command msg
    = Command
        { decoder : { usageSpecs : List UsageSpec, options : List ParsedOption, operands : List String } -> Result Cli.Decode.ProcessingError ( List Cli.Decode.ValidationError, msg )
        , usageSpecs : List UsageSpec
        , description : Maybe String
        , subCommand : Maybe String
        }


{-| TODO
-}
build : msg -> CommandBuilder msg
build msgConstructor =
    CommandBuilder
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        , subCommand = Nothing
        }


{-| TODO
-}
subCommand : String -> msg -> CommandBuilder msg
subCommand subCommandName msgConstructor =
    CommandBuilder
        { usageSpecs = []
        , description = Nothing
        , decoder = \_ -> Ok ( [], msgConstructor )
        , subCommand = Just subCommandName
        }


{-| TODO
-}
buildWithDoc : msg -> String -> CommandBuilder msg
buildWithDoc msgConstructor docString =
    CommandBuilder
        { usageSpecs = []
        , description = Just docString
        , decoder = \_ -> Ok ( [], msgConstructor )
        , subCommand = Nothing
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
            | usageSpecs = usageSpecs ++ [ UsageSpec.option (Flag flagName) Required ]
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


{-| TODO
-}
withDefault : value -> CliSpec (Maybe value) (Maybe value) -> CliSpec (Maybe value) value
withDefault defaultValue (CliSpec dataGrabber usageSpec decoder) =
    CliSpec dataGrabber usageSpec (decoder |> Cli.Decode.map (Maybe.withDefault defaultValue))


{-| TODO
-}
validate : (to -> ValidationResult) -> CliSpec from to -> CliSpec from to
validate validateFunction (CliSpec dataGrabber usageSpec (Cli.Decode.Decoder decodeFn)) =
    CliSpec dataGrabber
        usageSpec
        (Cli.Decode.Decoder
            (decodeFn
                >> (\result ->
                        Result.map
                            (\( validationErrors, value ) ->
                                case validateFunction value of
                                    Valid ->
                                        ( validationErrors, value )

                                    Invalid invalidReason ->
                                        ( validationErrors
                                            ++ [ { name = UsageSpec.name usageSpec
                                                 , invalidReason = invalidReason
                                                 , valueAsString = toString value
                                                 }
                                               ]
                                        , value
                                        )
                            )
                            result
                   )
            )
        )


{-| TODO
-}
with : CliSpec from to -> CommandBuilder (to -> msg) -> CommandBuilder msg
with (CliSpec dataGrabber usageSpec (Cli.Decode.Decoder decodeFn)) ((CommandBuilder ({ decoder, usageSpecs } as command)) as fullCommand) =
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
                        |> Result.andThen decodeFn
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


optionName : Option -> String
optionName option =
    case option of
        Flag flagName ->
            flagName

        OptionWithStringArg optionName ->
            optionName
