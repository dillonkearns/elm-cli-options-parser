port module Ports exposing (print, printAndExitFailure, printAndExitSuccess)


port print : String -> Cmd msg


port printAndExitFailure : String -> Cmd msg


port printAndExitSuccess : String -> Cmd msg
