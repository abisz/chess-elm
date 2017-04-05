port module TestRunner exposing (main)

import Test.Runner.Node exposing (TestProgram, run)
import Json.Encode exposing (Value)
import Test exposing (describe)
import HelperTest


main : TestProgram
main =
    run emit <|
        describe "Test Suite"
            [ HelperTest.allTests
            ]


port emit : ( String, Value ) -> Cmd msg
