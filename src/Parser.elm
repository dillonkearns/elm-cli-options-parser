module Parser exposing (OptionKind(..), ParsedOption(..), flagsAndOperands)

import Cli.UsageSpec exposing (UsageSpec)


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
            case String.toList firstArg of
                '-' :: '-' :: restOfFirstString ->
                    if Cli.UsageSpec.optionHasArg usageSpecs (restOfFirstString |> String.fromList) then
                        case restArgs of
                            secondArg :: afterSecondArg ->
                                flagsAndOperands_ usageSpecs
                                    afterSecondArg
                                    { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) (OptionWithArg secondArg) ]
                                    , operands = soFar.operands
                                    }

                            _ ->
                                flagsAndOperands_ usageSpecs
                                    restArgs
                                    { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) Flag ]
                                    , operands = soFar.operands
                                    }
                    else
                        flagsAndOperands_ usageSpecs
                            restArgs
                            { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) Flag ]
                            , operands = soFar.operands
                            }

                _ ->
                    flagsAndOperands_ usageSpecs
                        restArgs
                        { options = soFar.options
                        , operands = soFar.operands ++ [ firstArg ]
                        }
