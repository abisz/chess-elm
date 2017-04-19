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
                    testCastlingPosition "bra8;bke8;" (loc 0 2)
        , test "should return TopRight" <|
            \() ->
                Expect.equal TopRight <|
                    testCastlingPosition "bke8;brh8;" (loc 0 6)
        , test "should return BottomLeft" <|
            \() ->
                Expect.equal BottomLeft <|
                    testCastlingPosition "wra1;wke1;" (loc 7 2)
        , test "should return BottomRight" <|
            \() ->
                Expect.equal BottomRight <|
                    testCastlingPosition "wke1;wrh1;" (loc 7 6)
        ]


castlingTest : Test
castlingTest =
    describe "Castling"
        [ test "Left White Castling works" <|
            \() ->
                Expect.equal True <|
                    testMove "wra1;wke1;" (loc 7 4) (loc 7 2) King White
        , test "Left White Castling not allowed" <|
            \() ->
                Expect.equal False <|
                    testMove "wke1;" (loc 7 4) (loc 7 2) King White
        , test "Not Allowed if fields are not empty" <|
            \() ->
                Expect.equal False <|
                    testMove "wke1;wpc1;wra1;" (loc 7 4) (loc 7 2) King White
        , test "Only possible from start position" <|
            \() ->
                Expect.equal False <|
                    testMove "wra1;wke3;" (loc 5 4) (loc 5 2) King White
        , test "Not possible if King is in Check" <|
            \() ->
                Expect.equal False <|
                    testMove "wra1;wke1;bbb4;" (loc 7 4) (loc 7 2) King White
        , test "Right Black Castling works" <|
            \() ->
                Expect.equal True <|
                    testMove "bke8;brh8;" (loc 0 4) (loc 0 6) King Black
        , test "Left Black Castling not allowed if there is a Figure in betweend" <|
            \() ->
                Expect.equal False <|
                    testMove "bke8;bra8;bbb8;" (loc 0 4) (loc 0 2) King Black
        , test "Right Black Castling not allowed if there is a Figure in between" <|
            \() ->
                Expect.equal False <|
                    testMove "bke8;bbf8;brh8;" (loc 0 4) (loc 0 6) King Black
        , test "May not move thorugh attacked field" <|
            \() ->
                Expect.equal False <|
                    testMove "brd5;wra1;wke1;" (loc 7 4) (loc 7 2) King White
        ]


checkTest : Test
checkTest =
    describe "Check"
        [ test "Should be Check" <|
            \() ->
                Expect.equal True <|
                    isCheck (boardFromString "wka1;bbh8;") White
        , test "Shouldn't be Check" <|
            \() ->
                Expect.equal False <|
                    isCheck (boardFromString "bkg4;wpd2;") Black
        , test "Should be Check" <|
            \() ->
                Expect.equal True <|
                    isCheck (boardFromString "bba8;wng5;bka2;wkh1;") White
        , test "Shouldn't be Check" <|
            \() ->
                Expect.equal False <|
                    isCheck (boardFromString "bba8;wng5;bka2;wkh2;") White
        ]


checkMateTest : Test
checkMateTest =
    describe "Check Mate"
        [ test "Should be Check Mate" <|
            \() ->
                Expect.equal True <|
                    isCheckMate (boardFromString "wka1;")
        , test "Shouldn't be Check Mate" <|
            \() ->
                Expect.equal False <|
                    isCheckMate (boardFromString "bka2;wkd4;")
        ]


pawnMoveTest : Test
pawnMoveTest =
    describe "Pawn Moves"
        [ describe "Basic Moves"
            [ test "Default Move" <|
                \() ->
                    Expect.equal True <|
                        testMove "wpa4;" (loc 4 0) (loc 3 0) Pawn White
            , test "Can't move backwards" <|
                \() ->
                    Expect.equal False <|
                        testMove "wpa4;" (loc 4 0) (loc 5 0) Pawn White
            , test "Can't move diagonal" <|
                \() ->
                    Expect.equal False <|
                        testMove "wpa4;" (loc 4 0) (loc 3 1) Pawn White
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wpc4;wkb4;brd4;" (loc 4 2) (loc 5 2) Pawn White
            ]
        , describe "Double Move"
            [ test "Possible for First Move" <|
                \() ->
                    Expect.equal True <|
                        testMove "wpa2;" (loc 6 0) (loc 4 0) Pawn White
            , test "Not possible by Default" <|
                \() ->
                    Expect.equal False <|
                        testMove "wpa3;" (loc 5 0) (loc 2 0) Pawn White
            , test "Double Move can't skip figure" <|
                \() ->
                    Expect.equal False <|
                        testMove "wpa2;wpa3;" (loc 6 0) (loc 4 0) Pawn White
            , test "Can't combine Double Move with Beating" <|
                \() ->
                    Expect.equal False <|
                        testMove "bpc7;wpb5;" (loc 1 2) (loc 3 1) Pawn Black
            ]
        , describe "Beating"
            [ test "Can't beat straight" <|
                \() ->
                    Expect.equal False <|
                        testMove "bpd4;wpd3;" (loc 4 3) (loc 5 3) Pawn Black
            , test "Can't beat same color" <|
                \() ->
                    Expect.equal False <|
                        testMove "wpa2;wpa3;" (loc 6 0) (loc 5 0) Pawn White
            , test "Can't beat other color double straight" <|
                \() ->
                    Expect.equal False <|
                        testMove "wpa2;bpa4;" (loc 6 0) (loc 4 0) Pawn White
            , test "Can beat other color diagonal" <|
                \() ->
                    Expect.equal True <|
                        testMove "wpa2;bpb3;" (loc 6 0) (loc 5 1) Pawn White
            ]
        ]


knightMoveTest : Test
knightMoveTest =
    describe "Knight Moves"
        [ describe "Basic Moves"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "bnd5;" (loc 3 3) (loc 2 1) Knight Black
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bnd5;" (loc 3 3) (loc 3 4) Knight Black
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wnf3;wkh1;bba8;bka1;" (loc 5 5) (loc 3 6) Knight White
            ]
        , test "Jump over Figure" <|
            \() ->
                Expect.equal True <| testMove "bnd5;wpd6;wpe6;wpd7;" (loc 3 3) (loc 1 4) Knight Black
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bnd5;wpb4;" (loc 3 3) (loc 4 1) Knight Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "bnd5;bpf4;" (loc 3 3) (loc 4 5) Knight Black
            ]
        ]


bishopMoveTest : Test
bishopMoveTest =
    describe "Bishop Moves"
        [ describe "Basic Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wbb3;" (loc 5 1) (loc 2 4) Bishop White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bbc4;" (loc 4 2) (loc 4 0) Bishop Black
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "bbb4;bka4;wnc3;" (loc 4 1) (loc 3 2) Bishop Black
            ]
        , test "Jump Over Figure" <|
            \() ->
                Expect.equal False <|
                    testMove "wbe6;wpd7;" (loc 2 4) (loc 0 2) Bishop White
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bbd3;wkb5;" (loc 5 3) (loc 3 1) Bishop Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "wba8;wph1;" (loc 0 0) (loc 7 7) Bishop White
            ]
        ]


rookMoveTest : Test
rookMoveTest =
    describe "Rook Moves"
        [ describe "Basic Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wrc8;" (loc 0 2) (loc 0 7) Rook White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bre2;" (loc 6 4) (loc 4 3) Rook Black
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wra1;wke1;bqa5;" (loc 7 0) (loc 5 0) Rook White
            ]
        , test "Jump Over Figure" <|
            \() ->
                Expect.equal False <|
                    testMove "wrh5;bpf5;" (loc 3 7) (loc 3 3) Rook White
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bra1;wqe1;" (loc 7 0) (loc 7 4) Rook Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "brb8;bbb4;" (loc 0 1) (loc 4 1) Rook Black
            ]
        ]


queenMoveTest : Test
queenMoveTest =
    describe "Rook Moves"
        [ describe "Basic Rook Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wqc5;" (loc 3 2) (loc 3 7) Queen White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bqg8;" (loc 0 6) (loc 1 3) Queen Black
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "bqe5;bkg7;wpf6;" (loc 3 4) (loc 2 3) Queen Black
            ]
        , describe "Basic Bishop Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wqe2;" (loc 6 4) (loc 7 5) Queen White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bqf7;" (loc 1 5) (loc 5 6) Queen Black
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wqd1;wke1;bnf3;" (loc 7 3) (loc 4 0) Queen White
            ]
        , test "Jump Over Figure" <|
            \() ->
                Expect.equal False <|
                    testMove "wqh3;wpf5;" (loc 5 7) (loc 3 4) Queen White
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bqe1;wrc1;" (loc 7 4) (loc 7 2) Queen Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "wqg2;wkg5;" (loc 6 6) (loc 3 6) Queen White
            ]
        ]


kingMoveTest : Test
kingMoveTest =
    describe "King Moves"
        [ describe "Basic Move"
            [ test "Default Success" <|
                \() ->
                    Expect.equal True <|
                        testMove "wka1;" (loc 7 0) (loc 7 1) King White
            , test "Default Failure" <|
                \() ->
                    Expect.equal False <|
                        testMove "bkf4;" (loc 4 5) (loc 1 3) King Black
            , test "Can't cause Check for itself" <|
                \() ->
                    Expect.equal False <|
                        testMove "wkd3;bpe5;" (loc 5 3) (loc 4 3) King White
            ]
        , describe "Beat Figure"
            [ test "Different Color" <|
                \() ->
                    Expect.equal True <|
                        testMove "bkb5;wpa4;" (loc 3 1) (loc 4 0) King Black
            , test "Same Color" <|
                \() ->
                    Expect.equal False <|
                        testMove "bkf7;bne7;" (loc 1 5) (loc 1 4) King Black
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
            boardFromString "bra8;bke8;"

        targetField =
            Field targetLocation (getFieldColor targetLocation) Nothing
    in
        castlingPosition board targetField
