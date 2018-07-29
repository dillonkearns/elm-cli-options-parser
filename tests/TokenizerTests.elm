module TokenizerTests exposing (all)

import Cli.OptionsParser as OptionsParser
import Cli.Option as Option
import Expect exposing (Expectation)
import Test exposing (..)
import Tokenizer exposing (ParsedOption(..))


all : Test
all =
    describe "flags and operands extraction"
        [ test "recognizes empty operands and flags" <|
            \() ->
                expectFlagsAndOperands []
                    (OptionsParser.build (,)
                        |> OptionsParser.with (Option.requiredKeywordArg "first-name")
                        |> OptionsParser.with (Option.requiredKeywordArg "last-name")
                        |> OptionsParser.end
                    )
                    { options = [], operands = [] }
        , test "gets operand from the front" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand", "--verbose", "--dry-run" ]
                    (OptionsParser.build (,,)
                        |> OptionsParser.expectFlag "verbose"
                        |> OptionsParser.expectFlag "dry-run"
                        |> OptionsParser.end
                    )
                    { options = [ ParsedOption "verbose" Tokenizer.Flag, ParsedOption "dry-run" Tokenizer.Flag ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the back" <|
            \() ->
                expectFlagsAndOperands
                    [ "--verbose", "--dry-run", "operand" ]
                    (OptionsParser.build (,,)
                        |> OptionsParser.expectFlag "verbose"
                        |> OptionsParser.expectFlag "dry-run"
                        |> OptionsParser.end
                    )
                    { options = [ ParsedOption "verbose" Tokenizer.Flag, ParsedOption "dry-run" Tokenizer.Flag ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the front when args are used" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand", "--first-name", "Will", "--last-name", "Riker" ]
                    (OptionsParser.build (,)
                        |> OptionsParser.with (Option.requiredKeywordArg "first-name")
                        |> OptionsParser.with (Option.requiredKeywordArg "last-name")
                        |> OptionsParser.end
                    )
                    { options = [ ParsedOption "first-name" (Tokenizer.KeywordArg "Will"), ParsedOption "last-name" (Tokenizer.KeywordArg "Riker") ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the back when args are present" <|
            \() ->
                expectFlagsAndOperands
                    [ "--first-name", "Will", "--last-name", "Riker", "operand" ]
                    (OptionsParser.build (,)
                        |> OptionsParser.with (Option.requiredKeywordArg "first-name")
                        |> OptionsParser.with (Option.requiredKeywordArg "last-name")
                        |> OptionsParser.end
                    )
                    { options = [ ParsedOption "first-name" (Tokenizer.KeywordArg "Will"), ParsedOption "last-name" (Tokenizer.KeywordArg "Riker") ]
                    , operands = [ "operand" ]
                    }
        , test "new" <|
            \() ->
                expectFlagsAndOperands
                    [ "--last-name"
                    , "Troi"
                    , "--first-name"
                    , "Deanna"
                    ]
                    (OptionsParser.build (,)
                        |> OptionsParser.with (Option.requiredKeywordArg "first-name")
                        |> OptionsParser.with (Option.requiredKeywordArg "last-name")
                        |> OptionsParser.end
                    )
                    { options = [ ParsedOption "last-name" (Tokenizer.KeywordArg "Troi"), ParsedOption "first-name" (Tokenizer.KeywordArg "Deanna") ]
                    , operands = []
                    }
        , test "gets operand when there are no options" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand" ]
                    (OptionsParser.build identity
                        |> OptionsParser.with (Option.positionalArg "foo")
                        |> OptionsParser.end
                    )
                    { options = []
                    , operands = [ "operand" ]
                    }
        , test "gets options with --option=value syntax" <|
            \() ->
                expectFlagsAndOperands [ "--name=Picard" ]
                    (OptionsParser.build identity
                        |> OptionsParser.with (Option.requiredKeywordArg "name")
                        |> OptionsParser.end
                    )
                    { options = [ ParsedOption "name" (Tokenizer.KeywordArg "Picard") ], operands = [] }
        ]


expectFlagsAndOperands :
    List String
    -> OptionsParser.ActualOptionsParser decodesTo anything
    -> { options : List ParsedOption, operands : List String }
    -> Expectation
expectFlagsAndOperands argv optionsParser expected =
    Tokenizer.flagsAndOperands (OptionsParser.getUsageSpecs optionsParser) argv
        |> (\{ options, operands } -> { options = options, operands = operands })
        |> Expect.equal expected
