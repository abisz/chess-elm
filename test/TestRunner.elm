port module TestRunner exposing (main)

import Test.Runner.Node exposing (TestProgram, run)
import Json.Encode exposing (Value)
import Test exposing (describe)
import MoveTest


main : TestProgram
main =
    run emit <|
        describe "Test Suite"
            [ MoveTest.allTests
            ]


port emit : ( String, Value ) -> Cmd msg
