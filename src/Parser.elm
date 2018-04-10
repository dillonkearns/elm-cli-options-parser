module Parser exposing (OptionKind(..), ParsedOption(..), flagsAndOperands)

import Cli.UsageSpec exposing (UsageSpec)
import Parser.EqualsSplitter as EqualsSplitter


type ParsedOption
    = ParsedOption String OptionKind


type OptionKind
    = Flag
    | OptionWithArg String


flagsAndOperands : List UsageSpec -> List String -> { options : List ParsedOption, operands : List String }
flagsAndOperands usageSpecs argv =
    flagsAndOperands_ usageSpecs argv { options = [], operands = [] }


flagsAndOperands_ :
    List UsageSpec
    -> List String
    -> { options : List ParsedOption, operands : List String }
    -> { options : List ParsedOption, operands : List String }
flagsAndOperands_ usageSpecs argv soFar =
    case argv of
        [] ->
            soFar

        firstArg :: restArgs ->
            case EqualsSplitter.split firstArg of
                EqualsSplitter.Option optionName ->
                    if Cli.UsageSpec.optionHasArg usageSpecs optionName then
                        case restArgs of
                            secondArg :: afterSecondArg ->
                                flagsAndOperands_ usageSpecs
                                    afterSecondArg
                                    { options = soFar.options ++ [ ParsedOption optionName (OptionWithArg secondArg) ]
                                    , operands = soFar.operands
                                    }

                            _ ->
                                flagsAndOperands_ usageSpecs
                                    restArgs
                                    { options = soFar.options ++ [ ParsedOption optionName Flag ]
                                    , operands = soFar.operands
                                    }
                    else
                        flagsAndOperands_ usageSpecs
                            restArgs
                            { options = soFar.options ++ [ ParsedOption optionName Flag ]
                            , operands = soFar.operands
                            }

                EqualsSplitter.OptionWithArg { name, value } ->
                    flagsAndOperands_ usageSpecs
                        restArgs
                        { options = soFar.options ++ [ ParsedOption name (OptionWithArg value) ]
                        , operands = soFar.operands
                        }

                EqualsSplitter.NotOption ->
                    flagsAndOperands_ usageSpecs
                        restArgs
                        { options = soFar.options
                        , operands = soFar.operands ++ [ firstArg ]
                        }
