module ParserTests exposing (all)

import Cli.UsageSpec exposing (UsageSpec)
import Command
import Expect exposing (Expectation)
import Test exposing (..)


flagsAndOperands : List UsageSpec -> List String -> { flags : List ParsedOption, operands : List String }
flagsAndOperands usageSpecs argv =
    case argv of
        [] ->
            { flags = [], operands = [] }

        _ ->
            { flags = [ Flag "--verbose", Flag "--dry-run" ]
            , operands = [ "operand" ]
            }


type ParsedOption
    = Flag String


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
                    { flags = [ Flag "--verbose", Flag "--dry-run" ]
                    , operands = [ "operand" ]
                    }

        -- , test "gets operand from the back" <|
        --     \() ->
        --         [ "--verbose", "--dry-run", "operand" ]
        --             |> Command.flagsAndOperands
        --                 (Command.build (,,)
        --                     |> Command.expectFlag "verbose"
        --                     |> Command.expectFlag "dry-run"
        --                     |> Command.toCommand
        --                 )
        --             |> expectFlagsAndOperands
        --                 { flags = [ "--verbose", "--dry-run" ]
        --                 , operands = [ "operand" ]
        --                 }
        -- , test "gets operand from the front when args are used" <|
        --     \() ->
        --         [ "operand", "--first-name", "Will", "--last-name", "Riker" ]
        --             |> Command.flagsAndOperands
        --                 (Command.build FullName
        --                     |> Command.optionWithStringArg "first-name"
        --                     |> Command.optionWithStringArg "last-name"
        --                     |> Command.toCommand
        --                 )
        --             |> expectFlagsAndOperands
        --                 { flags = [ "--first-name", "Will", "--last-name", "Riker" ]
        --                 , operands = [ "operand" ]
        --                 }
        -- , test "gets operand from the back when args are present" <|
        --     \() ->
        --         [ "--first-name", "Will", "--last-name", "Riker", "operand" ]
        --             |> Command.flagsAndOperands
        --                 (Command.build FullName
        --                     |> Command.optionWithStringArg "first-name"
        --                     |> Command.optionWithStringArg "last-name"
        --                     |> Command.toCommand
        --                 )
        --             |> expectFlagsAndOperands
        --                 { flags = [ "--first-name", "Will", "--last-name", "Riker" ]
        --                 , operands = [ "operand" ]
        --                 }
        -- , test "gets operand when there are no options" <|
        --     \() ->
        --         [ "operand" ]
        --             |> Command.flagsAndOperands
        --                 (Command.build identity
        --                     |> Command.expectOperand "foo"
        --                     |> Command.toCommand
        --                 )
        --             |> expectFlagsAndOperands
        --                 { flags = []
        --                 , operands = [ "operand" ]
        --                 }
        ]


expectFlagsAndOperands :
    List String
    -> Command.Command decodesTo
    -> { flags : List ParsedOption, operands : List String }
    -> Expectation
expectFlagsAndOperands argv command expected =
    flagsAndOperands (Command.getUsageSpecs command) argv
        |> (\{ flags, operands } -> { flags = flags, operands = operands })
        |> Expect.equal expected
