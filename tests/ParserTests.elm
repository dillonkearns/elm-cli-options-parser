module ParserTests exposing (all)

import Command
import Expect exposing (Expectation)
import Parser exposing (ParsedOption(..))
import Test exposing (..)


all : Test
all =
    describe "flags and operands extraction"
        [ test "recognizes empty operands and flags" <|
            \() ->
                expectFlagsAndOperands []
                    (Command.build (,)
                        |> Command.with (Command.requiredOption "first-name")
                        |> Command.with (Command.requiredOption "last-name")
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
                    { options = [ ParsedOption "verbose" Parser.Flag, ParsedOption "dry-run" Parser.Flag ]
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
                    { options = [ ParsedOption "verbose" Parser.Flag, ParsedOption "dry-run" Parser.Flag ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the front when args are used" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand", "--first-name", "Will", "--last-name", "Riker" ]
                    (Command.build (,)
                        |> Command.with (Command.requiredOption "first-name")
                        |> Command.with (Command.requiredOption "last-name")
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "first-name" (Parser.OptionWithArg "Will"), ParsedOption "last-name" (Parser.OptionWithArg "Riker") ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the back when args are present" <|
            \() ->
                expectFlagsAndOperands
                    [ "--first-name", "Will", "--last-name", "Riker", "operand" ]
                    (Command.build (,)
                        |> Command.with (Command.requiredOption "first-name")
                        |> Command.with (Command.requiredOption "last-name")
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "first-name" (Parser.OptionWithArg "Will"), ParsedOption "last-name" (Parser.OptionWithArg "Riker") ]
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
                        |> Command.with (Command.requiredOption "first-name")
                        |> Command.with (Command.requiredOption "last-name")
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "last-name" (Parser.OptionWithArg "Troi"), ParsedOption "first-name" (Parser.OptionWithArg "Deanna") ]
                    , operands = []
                    }
        , test "gets operand when there are no options" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand" ]
                    (Command.build identity
                        |> Command.with (Command.requiredOperand "foo")
                        |> Command.toCommand
                    )
                    { options = []
                    , operands = [ "operand" ]
                    }
        , test "gets options with --option=value syntax" <|
            \() ->
                expectFlagsAndOperands [ "--name=Picard" ]
                    (Command.build identity
                        |> Command.with (Command.requiredOption "name")
                        |> Command.toCommand
                    )
                    { options = [ ParsedOption "name" (Parser.OptionWithArg "Picard") ], operands = [] }
        ]


expectFlagsAndOperands :
    List String
    -> Command.Command decodesTo
    -> { options : List ParsedOption, operands : List String }
    -> Expectation
expectFlagsAndOperands argv command expected =
    Parser.flagsAndOperands (Command.getUsageSpecs command) argv
        |> (\{ options, operands } -> { options = options, operands = operands })
        |> Expect.equal expected
