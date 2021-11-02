module CurlTest exposing (..)

import Cli.LowLevel
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.MatchResult exposing (MatchResult(..))
import Dict
import Expect exposing (Expectation)
import Regex exposing (Regex)
import Test exposing (..)


argsRegex : Regex
argsRegex =
    -- \s+|\s*'([^']*)'|\s*"([^"]*)"
    --"\\s+|\\s*'([^']*)'|\\s*\"([^\"]*)\""
    "\\s+|\\s*'([^']*)'|\\s*\"([^\"]*)\"|(\\S+)"
        --"\"[^\"\\\\]*(?:\\\\[\\S\\s][^\"\\\\]*)*\"|'[^'\\\\]*(?:\\[\\S\\s][^'\\]*)*'"
        -- source: https://stackoverflow.com/a/43766456
        --"\"([^\"\\\\]*(?:\\\\[\\S\\s][^\"\\]*)*)\"|'([^'\\]*(?:\\[\\S\\s][^'\\]*)*)'"
        |> regex


replaceEscapedNewlines : String -> String
replaceEscapedNewlines string =
    string
        |> String.replace "\\\n" " "


runCurl : String -> MatchResult { url : String, method : Method, headers : Dict.Dict String String }
runCurl command =
    OptionsParser.tryMatch
        (command
            |> replaceEscapedNewlines
            |> Regex.find argsRegex
            |> List.map
                (\match ->
                    match.submatches
                        |> List.head
                        |> Maybe.withDefault Nothing
                        |> Maybe.withDefault match.match
                )
            |> List.filter
                (\arg ->
                    arg
                        |> Regex.contains (regex "^\\s*$")
                        |> not
                )
        )
        curl


regex : String -> Regex
regex string =
    Regex.fromString string |> Maybe.withDefault Regex.never


splitHeader : String -> ( String, String )
splitHeader header =
    let
        index : Int
        index =
            header
                |> String.indexes ":"
                |> List.head
                |> Maybe.withDefault 0
    in
    ( String.left index header
    , String.dropLeft (index + 1) header
        |> removeLeadingSpace
    )


removeLeadingSpace : String -> String
removeLeadingSpace string =
    Regex.replace (regex "^\\s*") (\value -> "") string


type Method
    = Get
    | Post


curl =
    OptionsParser.build
        (\url data compressed header headers2 ->
            { url = url
            , method =
                if data == [] then
                    Get

                else
                    Post
            , headers =
                (header ++ headers2)
                    |> List.map splitHeader
                    |> Dict.fromList
            }
        )
        |> OptionsParser.with (Option.requiredPositionalArg "url")
        |> OptionsParser.with (Option.keywordArgList "data")
        |> OptionsParser.with (Option.flag "compressed")
        |> OptionsParser.with (Option.keywordArgList "header")
        --|> OptionsParser.with (Option.keywordArgList "header" |> Option.withAlias "H")
        |> OptionsParser.with (Option.keywordArgList "H")
        --|> OptionsParser.with (Option.requiredKeywordArg "first-name")
        --|> OptionsParser.with (Option.requiredKeywordArg "last-name")
        |> OptionsParser.end


suite : Test
suite =
    describe "curl"
        [ test "splitHeader" <|
            \() ->
                splitHeader "MyHeader: Abc123"
                    |> Expect.equal
                        ( "MyHeader", "Abc123" )
        , test "splitHeader2" <|
            \() ->
                splitHeader "Accept-Encoding: gzip, deflate, sdch"
                    |> Expect.equal
                        ( "Accept-Encoding", "gzip, deflate, sdch" )
        , test "get" <|
            \() ->
                runCurl """'http://en.wikipedia.org/' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Referer: http://www.wikipedia.org/'  -H 'Connection: keep-alive' --compressed"""
                    |> Expect.equal
                        (Match
                            (Ok
                                { url = "http://en.wikipedia.org/"
                                , method = Get
                                , headers =
                                    [ ( "Accept-Encoding", "gzip, deflate, sdch" )
                                    , ( "Accept-Language", "en-US,en;q=0.8" )
                                    , ( "User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36" )
                                    , ( "Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8" )
                                    , ( "Referer", "http://www.wikipedia.org/" )
                                    , ( "Connection", "keep-alive" )
                                    ]
                                        |> Dict.fromList
                                }
                            )
                        )
        , test "post" <|
            \() ->
                """'http://fiddle.jshell.net/echo/html/' -H 'Origin: http://fiddle.jshell.net' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36' -H 'Content-Type: application/x-www-form-urlencoded; charset=UTF-8' -H 'Accept: */*' -H 'Referer: http://fiddle.jshell.net/_display/' -H 'X-Requested-With: XMLHttpRequest' -H 'Connection: keep-alive' --data 'msg1=wow&msg2=such&msg3=data' --compressed"""
                    |> runCurl
                    |> Expect.equal
                        (Match
                            (Ok
                                { url = "http://fiddle.jshell.net/echo/html/"
                                , method = Post
                                , headers =
                                    [ ( "Origin", "http://fiddle.jshell.net" )
                                    , ( "Accept-Encoding", "gzip, deflate" )
                                    , ( "Accept-Language", "en-US,en;q=0.8" )
                                    , ( "User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36" )
                                    , ( "Content-Type", "application/x-www-form-urlencoded; charset=UTF-8" )
                                    , ( "Accept", "*/*" )
                                    , ( "Referer", "http://fiddle.jshell.net/_display/" )
                                    , ( "X-Requested-With", "XMLHttpRequest" )
                                    , ( "Connection", "keep-alive" )
                                    ]
                                        |> Dict.fromList
                                }
                            )
                        )
        , test "example from chrome dev tools copy" <|
            \() ->
                """'https://incrementalelm.com/manifest.json' \\
                     -H 'authority: incrementalelm.com' \\
                     -H 'pragma: no-cache' \\
                     -H 'cache-control: no-cache' \\
                     -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.54 Safari/537.36' \\
                     -H 'accept: */*' \\
                     -H 'sec-gpc: 1' \\
                     -H 'sec-fetch-site: same-origin' \\
                     -H 'sec-fetch-mode: cors' \\
                     -H 'sec-fetch-dest: manifest' \\
                     -H 'referer: https://incrementalelm.com/' \\
                     -H 'accept-language: en-US,en;q=0.9' \\
                     --compressed"""
                    |> runCurl
                    |> Expect.equal
                        (Match
                            (Ok
                                { headers =
                                    Dict.fromList
                                        [ ( "accept", "*/*" )
                                        , ( "accept-language", "en-US,en;q=0.9" )
                                        , ( "authority", "incrementalelm.com" )
                                        , ( "cache-control", "no-cache" )
                                        , ( "pragma", "no-cache" )
                                        , ( "referer", "https://incrementalelm.com/" )
                                        , ( "sec-fetch-dest", "manifest" )
                                        , ( "sec-fetch-mode", "cors" )
                                        , ( "sec-fetch-site", "same-origin" )
                                        , ( "sec-gpc", "1" )
                                        , ( "user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.54 Safari/537.36" )
                                        ]
                                , method = Get
                                , url = "https://incrementalelm.com/manifest.json"
                                }
                            )
                        )
        ]
