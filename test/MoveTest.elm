module MoveTest exposing (allTests)

import Test exposing (Test, describe, test)
import Types exposing (..)
import Matrix exposing (..)
import Move exposing (isMoveLegit, isCheckMate)
import BoardGenerator exposing (getFieldColor, boardFromString, fieldFromString)
import Expect


allTests : Test
allTests =
    describe "Move Test Suite"
        [ pawnMoveTest
        , knightMoveTest
        , bishopMoveTest
        , rookMoveTest
        , queenMoveTest
        , kingMoveTest
        , checkMateTest
        ]


checkMateTest : Test
checkMateTest =
    describe "Check Mate"
        [ test "Should be Check Mate" <|
            \() ->
                Expect.equal True <|
                    isCheckMate (boardFromString "wK1a;")
        , test "Shouldn't be Check Mate" <|
            \() ->
                Expect.equal False <|
                    isCheckMate (boardFromString "bK2a;wK4d;")
        ]


pawnMoveTest : Test
pawnMoveTest =
    describe "Pawn Moves"
        [ describe "Basic Moves"
            [ test "Default Move" <|
                \() ->
                    Expect.equal True <|
                        testMove "wp4a;" (loc 4 0) (loc 3 0) Pawn White
            , test "Can't move backwards" <|
                \() ->
                    Expect.equal False <|
                        testMove "wp4a;" (loc 4 0) (loc 5 0) Pawn White
            , test "Can't move diagonal" <|
                \() ->
                    Expect.equal False <|
                        testMove "wp4a;" (loc 4 0) (loc 3 1) Pawn White
            ]
        , describe "Double Move"
            [ test "Possible for First Move" <|
                \() ->
                    Expect.equal True <|
                        testMove "wp2a;" (loc 6 0) (loc 4 0) Pawn White
            , test "Not possible by Default" <|
                \() ->
                    Expect.equal False <|
                        testMove "wp3a;" (loc 5 0) (loc 2 0) Pawn White
            , test "Double Move can't skip figure" <|
                \() ->
                    Expect.equal False <|
                        testMove "wp2a;wp3a;" (loc 6 0) (loc 4 0) Pawn White
            , test "Can't combine Double Move with Beating" <|
                \() ->
                    Expect.equal False <|
                        testMove "bp7c;wp5b;" (loc 1 2) (loc 3 1) Pawn Black
            ]
        , describe "Beating"
            [ test "Can't beat straight" <|
                \() ->
                    Expect.equal False <|
                        testMove "bp4d;wp3d;" (loc 4 3) (loc 5 3) Pawn Black
            , test "Can't beat same color" <|
                \() ->
                    Expect.equal False <|
                        testMove "wp2a;wp3a;" (loc 6 0) (loc 5 0) Pawn White
            , test "Can't beat other color double straight" <|
                \() ->
                    Expect.equal False <|
                        testMove "wp2a;bp4a;" (loc 6 0) (loc 4 0) Pawn White
            , test "Can beat other color diagonal" <|
                \() ->
                    Expect.equal True <|
                        testMove "wp2a;bp3b;" (loc 6 0) (loc 5 1) Pawn White
            ]
        ]


knightMoveTest : Test
knightMoveTest =
    describe "Knight Moves"
        [ describe "Basic Moves"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "bk5d;" (loc 3 3) (loc 2 1) Knight Black
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bk5d;" (loc 3 3) (loc 3 4) Knight Black
            ]
        , test "Jump over Figure" <|
            \() ->
                Expect.equal True <| testMove "bk5d;wp6d;wp6e;wp7d;" (loc 3 3) (loc 1 4) Knight Black
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bk5d;wp4b;" (loc 3 3) (loc 4 1) Knight Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "bk5d;bp4f;" (loc 3 3) (loc 4 5) Knight Black
            ]
        ]


bishopMoveTest : Test
bishopMoveTest =
    describe "Bishop Moves"
        [ describe "Basic Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wb3b;" (loc 5 1) (loc 2 4) Bishop White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bb4c;" (loc 4 2) (loc 4 0) Bishop Black
            ]
        , test "Jump Over Figure" <|
            \() ->
                Expect.equal False <|
                    testMove "wb6e;wp7d;" (loc 2 4) (loc 0 2) Bishop White
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bb3d;wK5b;" (loc 5 3) (loc 3 1) Bishop Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "wb8a;wp1h;" (loc 0 0) (loc 7 7) Bishop White
            ]
        ]


rookMoveTest : Test
rookMoveTest =
    describe "Rook Moves"
        [ describe "Basic Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wr8c;" (loc 0 2) (loc 0 7) Rook White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "br2e;" (loc 6 4) (loc 4 3) Rook Black
            ]
        , test "Jump Over Figure" <|
            \() ->
                Expect.equal False <|
                    testMove "wr5h;bp5f;" (loc 3 7) (loc 3 3) Rook White
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "br1a;wq1e;" (loc 7 0) (loc 7 4) Rook Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "br8b;bb4b;" (loc 0 1) (loc 4 1) Rook Black
            ]
        ]


queenMoveTest : Test
queenMoveTest =
    describe "Rook Moves"
        [ describe "Basic Rook Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wq5c;" (loc 3 2) (loc 3 7) Queen White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bq8g;" (loc 0 6) (loc 1 3) Queen Black
            ]
        , describe "Basic Bishop Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wq2e;" (loc 6 4) (loc 7 5) Queen White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bq7f;" (loc 1 5) (loc 5 6) Queen Black
            ]
        , test "Jump Over Figure" <|
            \() ->
                Expect.equal False <|
                    testMove "wq3h;wp5f;" (loc 5 7) (loc 3 4) Queen White
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bq1e;wr1c;" (loc 7 4) (loc 7 2) Queen Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "wq2g;wK5g;" (loc 6 6) (loc 3 6) Queen White
            ]
        ]


kingMoveTest : Test
kingMoveTest =
    describe "King Moves"
        [ describe "Basic Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wK1a;" (loc 7 0) (loc 7 1) King White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bK4f;" (loc 4 5) (loc 1 3) King Black
            ]
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bK5b;wp4a;" (loc 3 1) (loc 4 0) King Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "bK7f;bk7e;" (loc 1 5) (loc 1 4) King Black
            ]
        ]


testMove : String -> Location -> Location -> ChessFigure -> Player -> Bool
testMove boardString selectedLocation targetLocation chessFigure player =
    let
        testBoard =
            boardFromString boardString

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
