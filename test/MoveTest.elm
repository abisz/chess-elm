module MoveTest exposing (allTests)

import Test exposing (Test, describe, test)
import Types exposing (..)
import Matrix exposing (..)
import Move exposing (isMoveLegit)
import BoardGenerator exposing (getFieldColor)
import Converter exposing (stringToBoard, stringToField)
import Expect


allTests : Test
allTests =
    describe "Move Test Suite"
        [ pawnMoveTest
        ]


pawnMoveTest : Test
pawnMoveTest =
    describe "Pawn Move Test"
        [ test "Default Move" <|
            \() ->
                Expect.equal True <| testMove "wp4a;" (loc 4 0) (loc 3 0) Pawn White
        , test "Can't move backwards" <|
            \() ->
                Expect.equal False <| testMove "wp4a;" (loc 4 0) (loc 5 0) Pawn White
        , test "Can't move diagonal" <|
            \() ->
                Expect.equal False <| testMove "wp4a;" (loc 4 0) (loc 3 1) Pawn White
        , test "Double First Move" <|
            \() ->
                Expect.equal True <| testMove "wp2a;" (loc 6 0) (loc 4 0) Pawn White
        , test "Double Move not possible" <|
            \() ->
                Expect.equal False <| testMove "wp3a;" (loc 5 0) (loc 2 0) Pawn White
        , test "Double Move can't skip figure" <|
            \() ->
                Expect.equal False <| testMove "wp2a;wp3a;" (loc 6 0) (loc 4 0) Pawn White
        , test "Can't beat same color" <|
            \() ->
                Expect.equal False <| testMove "wp2a;wp3a;" (loc 6 0) (loc 5 0) Pawn White
        , test "Can beat other color straight" <|
            \() ->
                Expect.equal True <| testMove "wp2a;bp3a;" (loc 6 0) (loc 5 0) Pawn White
        , test "Can beat other color diagonal" <|
            \() ->
                Expect.equal True <| testMove "wp2a;bp3b;" (loc 6 0) (loc 5 1) Pawn White
        ]


testMove : String -> Location -> Location -> ChessFigure -> Player -> Bool
testMove boardString selectedLocation targetLocation chessFigure player =
    let
        testBoard =
            stringToBoard boardString

        selectedField =
            { loc = selectedLocation
            , color = (getFieldColor selectedLocation)
            , figure =
                Just
                    { figure = chessFigure
                    , color = player
                    }
            }

        targetField =
            case Matrix.get targetLocation testBoard of
                Just field ->
                    field

                Nothing ->
                    { loc = targetLocation
                    , color = (getFieldColor targetLocation)
                    , figure = Nothing
                    }
    in
        isMoveLegit testBoard (Active selectedField) targetField
