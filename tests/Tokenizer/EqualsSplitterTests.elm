module Tokenizer.EqualsSplitterTests exposing (all)

import Expect exposing (Expectation)
import Tokenizer.EqualsSplitter as EqualsSplitter
import Test exposing (..)


type Msg
    = Help
    | Version
    | OpenUrl String
    | OpenUrlWithFlag String Bool
    | Name String
    | FullName String String


all : Test
all =
    describe "equals splitter"
        [ test "not option" <|
            \() ->
                "operand"
                    |> EqualsSplitter.split
                    |> Expect.equal EqualsSplitter.NotOption
        , test "option without arg" <|
            \() ->
                "--name"
                    |> EqualsSplitter.split
                    |> Expect.equal (EqualsSplitter.Option "name")
        , test "option with arg" <|
            \() ->
                "--name=Picard"
                    |> EqualsSplitter.split
                    |> Expect.equal
                        (EqualsSplitter.OptionWithArg
                            { name = "name", value = "Picard" }
                        )
        ]
