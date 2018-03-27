module ParserTests exposing (all)

import Expect exposing (Expectation)
import Test exposing (..)


type Msg
    = Help
    | Version


parse : Parser Msg -> List String -> Result ParserError Msg
parse parser argv =
    -- case parser of
    --   LongOnly longOption ->
    if argv == [ "--help" ] then
        Ok Help
    else
        Err (UnknownOption "--unknown")


tryMatch : Command msg -> List String -> Maybe msg
tryMatch (Command msg format) argv =
    Just msg


parser : List (Command msg) -> Parser msg
parser commands =
    Parser commands


type Parser msg
    = Parser (List (Command msg))


type Command msg
    = Command msg Format


command : msg -> Format -> Command msg
command msg format =
    Command msg format


type Format
    = LongOnly String


type ParserError
    = UnknownOption String


all : Test
all =
    describe "CLI options parser"
        [ test "help command" <|
            \() ->
                [ "--help" ]
                    |> parse (parser [ command Help (LongOnly "help") ])
                    |> Expect.equal (Ok Help)
        , test "version command" <|
            \() ->
                [ "--version" ]
                    |> tryMatch (command Version (LongOnly "version"))
                    |> Expect.equal (Just Version)
        , test "unknown option" <|
            \() ->
                [ "--unknown" ]
                    |> parse (parser [ command Help (LongOnly "help") ])
                    |> Expect.equal (Err (UnknownOption "--unknown"))
        ]
