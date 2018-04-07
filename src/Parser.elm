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

        first :: second :: rest ->
            case String.toList first of
                '-' :: '-' :: restOfFirstString ->
                    if Cli.UsageSpec.optionHasArg usageSpecs (restOfFirstString |> String.fromList) then
                        flagsAndOperands_ usageSpecs
                            rest
                            { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) (OptionWithArg second) ]
                            , operands = soFar.operands
                            }
                    else
                        flagsAndOperands_ usageSpecs
                            (second :: rest)
                            { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) Flag ]
                            , operands = soFar.operands
                            }

                _ ->
                    flagsAndOperands_ usageSpecs
                        (second :: rest)
                        { options = soFar.options
                        , operands = soFar.operands ++ [ first ]
                        }

        first :: rest ->
            case String.toList first of
                '-' :: '-' :: restOfFirstString ->
                    flagsAndOperands_ usageSpecs
                        rest
                        { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) Flag ]
                        , operands = soFar.operands
                        }

                _ ->
                    flagsAndOperands_ usageSpecs
                        rest
                        { options = soFar.options
                        , operands = soFar.operands ++ [ first ]
                        }
