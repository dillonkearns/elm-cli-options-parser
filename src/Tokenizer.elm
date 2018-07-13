module Tokenizer exposing (OptionKind(..), ParsedOption(..), flagsAndOperands)

import Cli.UsageSpec exposing (UsageSpec)
import Tokenizer.EqualsSplitter as EqualsSplitter


type ParsedOption
    = ParsedOption String OptionKind


type OptionKind
    = Flag
    | KeywordArg String


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
                                    { options = soFar.options ++ [ ParsedOption optionName (KeywordArg secondArg) ]
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

                EqualsSplitter.KeywordArg { name, value } ->
                    flagsAndOperands_ usageSpecs
                        restArgs
                        { options = soFar.options ++ [ ParsedOption name (KeywordArg value) ]
                        , operands = soFar.operands
                        }

                EqualsSplitter.NotOption ->
                    flagsAndOperands_ usageSpecs
                        restArgs
                        { options = soFar.options
                        , operands = soFar.operands ++ [ firstArg ]
                        }
