port module Ports exposing (..)


port print : String -> Cmd msg


port printAndExitFailure : String -> Cmd msg


port printAndExitSuccess : String -> Cmd msg


port stdin : (String -> msg) -> Sub msg
