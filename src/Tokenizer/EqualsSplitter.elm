module Tokenizer.EqualsSplitter exposing (SplitResult(..), split)


type SplitResult
    = Option String
    | KeywordArg { name : String, value : String }
    | NotOption


split : String -> SplitResult
split string =
    case String.toList string of
        '-' :: '-' :: optionName ->
            case String.split "=" (optionName |> String.fromList) of
                [ singleOptionName ] ->
                    singleOptionName
                        |> Option

                firstOptionName :: splitAfterOptionName ->
                    KeywordArg { name = firstOptionName, value = String.concat splitAfterOptionName }

                _ ->
                    optionName
                        |> String.fromList
                        |> Option

        '-' :: optionName ->
            case String.split "=" (optionName |> String.fromList) of
                [ singleOptionName ] ->
                    singleOptionName
                        |> Option

                firstOptionName :: splitAfterOptionName ->
                    KeywordArg { name = firstOptionName, value = String.concat splitAfterOptionName }

                _ ->
                    optionName
                        |> String.fromList
                        |> Option

        _ ->
            NotOption
