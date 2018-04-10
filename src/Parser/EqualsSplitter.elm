module Parser.EqualsSplitter exposing (SplitResult(..), split)


type SplitResult
    = Option String
    | OptionWithArg { name : String, value : String }
    | NotOption


split : String -> SplitResult
split string =
    case String.toList string of
        '-' :: '-' :: optionName ->
            case String.split "=" (optionName |> String.fromList) of
                [ optionName ] ->
                    optionName
                        |> Option

                optionName :: splitAfterOptionName ->
                    OptionWithArg { name = optionName, value = String.concat splitAfterOptionName }

                _ ->
                    optionName
                        |> String.fromList
                        |> Option

        _ ->
            NotOption
