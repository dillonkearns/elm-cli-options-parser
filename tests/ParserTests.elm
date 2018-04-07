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
                        |> Command.optionWithStringArg "first-name"
                        |> Command.optionWithStringArg "last-name"
                        |> Command.toCommand
                    )
                    { flags = [], operands = [] }
        , test "gets operand from the front" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand", "--verbose", "--dry-run" ]
                    (Command.build (,,)
                        |> Command.expectFlag "verbose"
                        |> Command.expectFlag "dry-run"
                        |> Command.toCommand
                    )
                    { flags = [ Flag "verbose", Flag "dry-run" ]
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
                    { flags = [ Flag "verbose", Flag "dry-run" ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the front when args are used" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand", "--first-name", "Will", "--last-name", "Riker" ]
                    (Command.build (,)
                        |> Command.optionWithStringArg "first-name"
                        |> Command.optionWithStringArg "last-name"
                        |> Command.toCommand
                    )
                    { flags = [ Option "first-name" "Will", Option "last-name" "Riker" ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand from the back when args are present" <|
            \() ->
                expectFlagsAndOperands
                    [ "--first-name", "Will", "--last-name", "Riker", "operand" ]
                    (Command.build (,)
                        |> Command.optionWithStringArg "first-name"
                        |> Command.optionWithStringArg "last-name"
                        |> Command.toCommand
                    )
                    { flags = [ Option "first-name" "Will", Option "last-name" "Riker" ]
                    , operands = [ "operand" ]
                    }
        , test "gets operand when there are no options" <|
            \() ->
                expectFlagsAndOperands
                    [ "operand" ]
                    (Command.build identity
                        |> Command.expectOperand "foo"
                        |> Command.toCommand
                    )
                    { flags = []
                    , operands = [ "operand" ]
                    }
        ]


expectFlagsAndOperands :
    List String
    -> Command.Command decodesTo
    -> { flags : List ParsedOption, operands : List String }
    -> Expectation
expectFlagsAndOperands argv command expected =
    Parser.flagsAndOperands (Command.getUsageSpecs command) argv
        |> (\{ flags, operands } -> { flags = flags, operands = operands })
        |> Expect.equal expected
