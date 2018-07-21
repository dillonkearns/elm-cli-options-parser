module Cli.ExitStatus exposing (ExitStatus(Failure, Success))

{-|

@docs ExitStatus

-}


{-| On `Failure`, the program should return an exit code of `1`.
On success it should return an exit code of `0` (in command line interfaces,
`0` exit status means success, non-zero means there was an error).

Possible reasons for error include command not found, validtion error in command, etc.
See the examples folder for Elm command line programs with an end-to-end setup.

-}
type ExitStatus
    = Success
    | Failure
