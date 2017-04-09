module BoardGeneratorTest exposing (allTests)

import Test exposing (Test, describe, test)
import Expect
import BoardGenerator exposing (startBoard)
import Converter exposing (boardToString)


allTests : Test
allTests =
    describe "Board Generator Tests"
        [ startBoardTest
        ]


startBoardTest : Test
startBoardTest =
    describe "Start Board Generation"
        [ test "Correct Output" <|
            \() ->
                Expect.equal True <|
                    "br8a;bk8b;bb8c;bq8d;bK8e;bb8f;bk8g;br8h;bp7a;bp7b;bp7c;bp7d;bp7e;bp7f;bp7g;bp7h;wp2a;wp2b;wp2c;wp2d;wp2e;wp2f;wp2g;wp2h;wr1a;wk1b;wb1c;wq1d;wK1e;wb1f;wk1g;wr1h;"
                        == boardToString startBoard
        ]
