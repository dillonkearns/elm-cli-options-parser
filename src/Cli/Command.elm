module Cli.Command
    exposing
        ( Command
        , CommandBuilder
        , MatchResult(..)
        , build
        , buildWithDoc
        , captureRestOperands
        , expectFlag
        , getSubCommand
        , getUsageSpecs
        , hardcoded
        , map
        , mapNew
        , matchResultToMaybe
        , subCommand
        , synopsis
        , toCommand
        , tryMatch
        , tryMatchNew
        , with
        , withDefault
        )

{-| TODO
@docs Command, MatchResult

@docs build, buildWithDoc, captureRestOperands, expectFlag, getSubCommand, getUsageSpecs, hardcoded, map, mapNew, subCommand, synopsis, toCommand, tryMatch, tryMatchNew, with, withDefault

Low-level???
@docs CommandBuilder, matchResultToMaybe

-}

import Cli.Decode
import Cli.Spec exposing (CliSpec(..))
import Cli.UsageSpec as UsageSpec exposing (..)
import Cli.Validate exposing (ValidationResult(Invalid, Valid))
import Occurences exposing (Occurences(..))
import Parser exposing (ParsedOption)


{-| TODO
-}
type MatchResult msg
    = Match (Result (List Cli.Decode.ValidationError) msg)
    | NoMatch (List String)


{-| TODO
-}
matchResultToMaybe : MatchResult msg -> Maybe (Result (List Cli.Decode.ValidationError) msg)
matchResultToMaybe matchResult =
    case matchResult of
        Match thing ->
            Just thing

        NoMatch unknownFlags ->
            Nothing


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
            Parser.flagsAndOperands usageSpecs argv
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
                                        NoMatch []

                                    Cli.Decode.UnrecoverableValidationError validationError ->
                                        Match (Err [ validationError ])

                                    Cli.Decode.UnexpectedOptions unexpectedOptions ->
                                        NoMatch unexpectedOptions

                            Ok ( [], value ) ->
                                Match (Ok value)

                            Ok ( validationErrors, value ) ->
                                Match (Err validationErrors)
                   )

        Err { errorMessage, options } ->
            NoMatch (unexpectedOptions_ command options)


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
            Parser.flagsAndOperands usageSpecs argv
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
        (\(Parser.ParsedOption optionName optionKind) ->
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
toCommand : CommandBuilder msg -> Command msg
toCommand (CommandBuilder record) =
    Command record


{-| TODO
-}
map : (msg -> mappedMsg) -> Command msg -> Command mappedMsg
map mapFunction (Command ({ decoder } as record)) =
    Command { record | decoder = decoder >> Result.map (Tuple.mapSecond mapFunction) }


{-| TODO
-}
captureRestOperands : String -> CommandBuilder (List String -> msg) -> Command msg
captureRestOperands restOperandsDescription (CommandBuilder ({ usageSpecs, description, decoder } as record)) =
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
                            |> List.member (Parser.ParsedOption flagName Parser.Flag)
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
mapNew : (toRaw -> toMapped) -> CliSpec from toRaw -> CliSpec from toMapped
mapNew mapFn (CliSpec dataGrabber usageSpec ((Cli.Decode.Decoder decodeFn) as decoder)) =
    CliSpec dataGrabber usageSpec (Cli.Decode.map mapFn decoder)


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
