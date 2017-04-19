module MoveTest exposing (allTests)

import Test exposing (Test, describe, test)
import Types exposing (..)
import Matrix exposing (..)
import Move exposing (isMoveLegit, isCheckMate, isCheck, castlingPosition)
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
        , checkTest
        , castlingTest
        , castlingPositionTest
        ]


castlingPositionTest : Test
castlingPositionTest =
    describe "Castling Position"
        [ test "should return TopLeft" <|
            \() ->
                Expect.equal TopLeft <|
                    testCastlingPosition "br8a;bk8e;" (loc 0 2)
        , test "should return TopRight" <|
            \() ->
                Expect.equal TopRight <|
                    testCastlingPosition "bk8e;br8h;" (loc 0 6)
        , test "should return BottomLeft" <|
            \() ->
                Expect.equal BottomLeft <|
                    testCastlingPosition "wr1a;wk1e;" (loc 7 2)
        , test "should return BottomRight" <|
            \() ->
                Expect.equal BottomRight <|
                    testCastlingPosition "wk1e;wr1h;" (loc 7 6)
        ]


castlingTest : Test
castlingTest =
    describe "Castling"
        [ test "Left White Castling works" <|
            \() ->
                Expect.equal True <|
                    testMove "wr1a;wk1e;" (loc 7 4) (loc 7 2) King White
        , test "Left White Castling not allowed" <|
            \() ->
                Expect.equal False <|
                    testMove "wk1e;" (loc 7 4) (loc 7 2) King White
        , test "Not Allowed if fields are not empty" <|
            \() ->
                Expect.equal False <|
                    testMove "wk1e;wp1c;wr1a;" (loc 7 4) (loc 7 2) King White
        , test "Only possible from start position" <|
            \() ->
                Expect.equal False <|
                    testMove "wr1a;wk3e;" (loc 5 4) (loc 5 2) King White
        , test "Not possible if King is in Check" <|
            \() ->
                Expect.equal False <|
                    testMove "wr1a;wk1e;bb4b;" (loc 7 4) (loc 7 2) King White
        , test "Right Black Castling works" <|
            \() ->
                Expect.equal True <|
                    testMove "bk8e;br8h;" (loc 0 4) (loc 0 6) King Black
        , test "Left Black Castling not allowed if there is a Figure in betweend" <|
            \() ->
                Expect.equal False <|
                    testMove "bk8e;br8a;bb8b;" (loc 0 4) (loc 0 2) King Black
        , test "Right Black Castling not allowed if there is a Figure in between" <|
            \() ->
                Expect.equal False <|
                    testMove "bk8e;bb8f;br8h;" (loc 0 4) (loc 0 6) King Black
        , test "May not move thorugh attacked field" <|
            \() ->
                Expect.equal False <|
                    testMove "br5d;wr1a;wk1e;" (loc 7 4) (loc 7 2) King White
        ]


checkTest : Test
checkTest =
    describe "Check"
        [ test "Should be Check" <|
            \() ->
                Expect.equal True <|
                    isCheck (boardFromString "wk1a;bb8h;") White
        , test "Shouldn't be Check" <|
            \() ->
                Expect.equal False <|
                    isCheck (boardFromString "bk4g;wp2d;") Black
        , test "Should be Check" <|
            \() ->
                Expect.equal True <|
                    isCheck (boardFromString "bb8a;wn5g;bk2a;wk1h;") White
        , test "Shouldn't be Check" <|
            \() ->
                Expect.equal False <|
                    isCheck (boardFromString "bb8a;wn5g;bk2a;wk2h;") White
        ]


checkMateTest : Test
checkMateTest =
    describe "Check Mate"
        [ test "Should be Check Mate" <|
            \() ->
                Expect.equal True <|
                    isCheckMate (boardFromString "wk1a;")
        , test "Shouldn't be Check Mate" <|
            \() ->
                Expect.equal False <|
                    isCheckMate (boardFromString "bk2a;wk4d;")
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
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wp4c;wk4b;br4d;" (loc 4 2) (loc 5 2) Pawn White
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
                        testMove "bn5d;" (loc 3 3) (loc 2 1) Knight Black
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bn5d;" (loc 3 3) (loc 3 4) Knight Black
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wn3f;wk1h;bb8a;bk1a;" (loc 5 5) (loc 3 6) Knight White
            ]
        , test "Jump over Figure" <|
            \() ->
                Expect.equal True <| testMove "bn5d;wp6d;wp6e;wp7d;" (loc 3 3) (loc 1 4) Knight Black
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bn5d;wp4b;" (loc 3 3) (loc 4 1) Knight Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "bn5d;bp4f;" (loc 3 3) (loc 4 5) Knight Black
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
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "bb4b;bk4a;wn3c;" (loc 4 1) (loc 3 2) Bishop Black
            ]
        , test "Jump Over Figure" <|
            \() ->
                Expect.equal False <|
                    testMove "wb6e;wp7d;" (loc 2 4) (loc 0 2) Bishop White
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bb3d;wk5b;" (loc 5 3) (loc 3 1) Bishop Black
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
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wr1a;wk1e;bq5a;" (loc 7 0) (loc 5 0) Rook White
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
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "bq5e;bk7g;wp6f;" (loc 3 4) (loc 2 3) Queen Black
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
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wq1d;wk1e;bn3f;" (loc 7 3) (loc 4 0) Queen White
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
                        testMove "wq2g;wk5g;" (loc 6 6) (loc 3 6) Queen White
            ]
        ]


kingMoveTest : Test
kingMoveTest =
    describe "King Moves"
        [ describe "Basic Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wk1a;" (loc 7 0) (loc 7 1) King White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bk4f;" (loc 4 5) (loc 1 3) King Black
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wk3d;bp5e;" (loc 5 3) (loc 4 3) King White
            ]
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bk5b;wp4a;" (loc 3 1) (loc 4 0) King Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "bk7f;bn7e;" (loc 1 5) (loc 1 4) King Black
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
        isMoveLegit testBoard (Just selectedField) targetField


testCastlingPosition : String -> Location -> CastlingPosition
testCastlingPosition boardString targetLocation =
    let
        board =
            boardFromString "br8a;bk8e;"

        targetField =
            Field targetLocation (getFieldColor targetLocation) Nothing
    in
        castlingPosition board targetField
