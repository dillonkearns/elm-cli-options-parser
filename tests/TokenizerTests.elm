module TokenizerTests exposing (all)

import Cli.Command as Command
import Cli.Spec as Spec
import Expect exposing (Expectation)
import Test exposing (..)
import Tokenizer exposing (ParsedOption(..))


all : Test
all =
    describe "flags and operands extraction"
        [ test "recognizes empty operands and flags" <|
            \() ->
                expectFlagsAndOperands []
                    (Command.build (,)
                        |> Command.with (Spec.requiredKeywordArg "first-name")
                        |> Command.with (Spec.requiredKeywordArg "last-name")
                        |> Command.toCommand
                    )
                    { options = [], operands = [] }
        , test "gets operand from the front" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand", "--verbose", "--dry-run" ]
                    (Command.build (,,)
                        |> Command.expectFlag "verbose"
                        |> Command.expectFlag "dry-run"
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "verbose" Tokenizer.Flag, ParsedOption "dry-run" Tokenizer.Flag ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the back" <|
            \() ->
                expectFlagsAndOperands
                    [ "--verbose", "--dry-run", "operand" ]
                    (Command.build (,,)
                        |> Command.expectFlag "verbose"
                        |> Command.expectFlag "dry-run"
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "verbose" Tokenizer.Flag, ParsedOption "dry-run" Tokenizer.Flag ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the front when args are used" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand", "--first-name", "Will", "--last-name", "Riker" ]
                    (Command.build (,)
                        |> Command.with (Spec.requiredKeywordArg "first-name")
                        |> Command.with (Spec.requiredKeywordArg "last-name")
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "first-name" (Tokenizer.KeywordArg "Will"), ParsedOption "last-name" (Tokenizer.KeywordArg "Riker") ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the back when args are present" <|
            \() ->
                expectFlagsAndOperands
                    [ "--first-name", "Will", "--last-name", "Riker", "operand" ]
                    (Command.build (,)
                        |> Command.with (Spec.requiredKeywordArg "first-name")
                        |> Command.with (Spec.requiredKeywordArg "last-name")
                        |> Command.toCommand
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
                    (Command.build (,)
                        |> Command.with (Spec.requiredKeywordArg "first-name")
                        |> Command.with (Spec.requiredKeywordArg "last-name")
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "last-name" (Tokenizer.KeywordArg "Troi"), ParsedOption "first-name" (Tokenizer.KeywordArg "Deanna") ]
                    , operands = []
                    }
        , test "gets operand when there are no options" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand" ]
                    (Command.build identity
                        |> Command.with (Spec.positionalArg "foo")
                        |> Command.toCommand
                    )
                    { options = []
                    , operands = [ "operand" ]
                    }
        , test "gets options with --option=value syntax" <|
            \() ->
                expectFlagsAndOperands [ "--name=Picard" ]
                    (Command.build identity
                        |> Command.with (Spec.requiredKeywordArg "name")
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "name" (Tokenizer.KeywordArg "Picard") ], operands = [] }
        ]


expectFlagsAndOperands :
    List String
    -> Command.Command decodesTo
    -> { options : List ParsedOption, operands : List String }
    -> Expectation
expectFlagsAndOperands argv command expected =
    Tokenizer.flagsAndOperands (Command.getUsageSpecs command) argv
        |> (\{ options, operands } -> { options = options, operands = operands })
        |> Expect.equal expected
