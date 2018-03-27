module ParserTests exposing (all)

import Expect exposing (Expectation)
import Test exposing (..)


type Msg
    = Help


parse : Parser Msg -> List String -> Result ParserError Msg
parse parser argv =
    if argv == [ "--help" ] then
        Ok Help
    else
        Err (UnknownOption "--unknown")


parser : List (Command msg) -> Parser msg
parser commands =
    Parser commands


type Parser msg
    = Parser (List (Command msg))


type Command msg
    = Command msg


command : msg -> Format -> Command msg
command msg format =
    Command msg


type Format
    = LongOnly String


type ParserError
    = UnknownOption String


all : Test
all =
    describe "CLI options parser"
        [ test "help option" <|
            \() ->
                [ "--help" ]
                    |> parse (parser [ command Help (LongOnly "help") ])
                    |> Expect.equal (Ok Help)
        , test "unknown option" <|
            \() ->
                [ "--unknown" ]
                    |> parse (parser [ command Help (LongOnly "help") ])
                    |> Expect.equal (Err (UnknownOption "--unknown"))
        ]
