module HelperTest exposing (allTests)

import Test exposing (Test, describe, test)
import Expect
import Helper exposing (colorToCssString)
import Color exposing (Color)


allTests : Test
allTests =
    describe "Helper Tests"
        [ colorToCssStringTest
        ]


colorToCssStringTest : Test
colorToCssStringTest =
    describe "Color to CSS String"
        [ test "expected result for black" <|
            \() ->
                colorToCssString Color.black
                    |> Expect.equal "rgb(0, 0, 0)"
        , test "expected result for white" <|
            \() ->
                colorToCssString Color.white
                    |> Expect.equal "rgb(255, 255, 255)"
        , test "expected result for red" <|
            \() ->
              colorToCssString Color.red
                |> Expect.equal "rgb(204, 0, 0)"
        ]
