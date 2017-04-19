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
                    "bra8;bnb8;bbc8;bqd8;bke8;bbf8;bng8;brh8;bpa7;bpb7;bpc7;bpd7;bpe7;bpf7;bpg7;bph7;wpa2;wpb2;wpc2;wpd2;wpe2;wpf2;wpg2;wph2;wra1;wnb1;wbc1;wqd1;wke1;wbf1;wng1;wrh1;"
                        == boardToString startBoard
        ]
