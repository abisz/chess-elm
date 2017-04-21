module ConverterTest exposing (allTests)

import Test exposing (Test, describe, test)
import Expect
import Converter exposing (boardToFen)
import BoardGenerator exposing (startBoard)


allTests : Test
allTests =
    describe "Converter Tests"
        [ fenTest
        ]


fenTest : Test
fenTest =
    describe "fen tests"
        [ describe "boardToFen"
            [ test "correct startBoard" <|
                \() ->
                    Expect.equal "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" <|
                        boardToFen startBoard
            ]
        ]
