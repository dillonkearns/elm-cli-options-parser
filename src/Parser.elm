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

        first :: rest ->
            case String.toList first of
                '-' :: '-' :: restOfFirstString ->
                    if Cli.UsageSpec.optionHasArg (usageSpecs |> Debug.log "SPECS") (restOfFirstString |> String.fromList |> Debug.log "name") |> Debug.log "hasArg" then
                        case rest of
                            second :: subRest ->
                                let
                                    _ =
                                        Debug.log first "1"
                                in
                                flagsAndOperands_ usageSpecs
                                    subRest
                                    { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) (OptionWithArg second) ]
                                    , operands = soFar.operands
                                    }

                            _ ->
                                let
                                    _ =
                                        Debug.log first "2"
                                in
                                flagsAndOperands_ usageSpecs
                                    rest
                                    { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) Flag ]
                                    , operands = soFar.operands
                                    }
                    else
                        let
                            _ =
                                Debug.log first "3"
                        in
                        flagsAndOperands_ usageSpecs
                            rest
                            { options = soFar.options ++ [ ParsedOption (restOfFirstString |> String.fromList) Flag ]
                            , operands = soFar.operands
                            }

                _ ->
                    let
                        _ =
                            Debug.log first "4"
                    in
                    flagsAndOperands_ usageSpecs
                        rest
                        { options = soFar.options
                        , operands = soFar.operands ++ [ first ]
                        }
