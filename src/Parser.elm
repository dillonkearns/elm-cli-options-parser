module Parser exposing (ParsedOption(..), flagsAndOperands)

import Cli.UsageSpec exposing (UsageSpec)


type ParsedOption
    = Flag String
    | Option String String


flagsAndOperands : List UsageSpec -> List String -> { flags : List ParsedOption, operands : List String }
flagsAndOperands usageSpecs argv =
    flagsAndOperands_ usageSpecs argv { flags = [], operands = [] }


flagsAndOperands_ :
    List UsageSpec
    -> List String
    -> { flags : List ParsedOption, operands : List String }
    -> { flags : List ParsedOption, operands : List String }
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
                            { flags = soFar.flags ++ [ Option (restOfFirstString |> String.fromList) second ]
                            , operands = soFar.operands
                            }
                    else
                        flagsAndOperands_ usageSpecs
                            (second :: rest)
                            { flags = soFar.flags ++ [ Flag (restOfFirstString |> String.fromList) ]
                            , operands = soFar.operands
                            }

                _ ->
                    flagsAndOperands_ usageSpecs
                        (second :: rest)
                        { flags = soFar.flags
                        , operands = soFar.operands ++ [ first ]
                        }

        first :: rest ->
            case String.toList first of
                '-' :: '-' :: restOfFirstString ->
                    flagsAndOperands_ usageSpecs
                        rest
                        { flags = soFar.flags ++ [ Flag (restOfFirstString |> String.fromList) ]
                        , operands = soFar.operands
                        }

                _ ->
                    flagsAndOperands_ usageSpecs
                        rest
                        { flags = soFar.flags
                        , operands = soFar.operands ++ [ first ]
                        }
