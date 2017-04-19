module Move exposing (isMoveLegit, isCheckMate, isCheck, isCastlingMove, castlingPosition)

import Types exposing (..)
import Matrix exposing (..)
import Converter exposing (boardToString)
import BoardGenerator exposing (getFieldColor)


isCheck : Matrix Field -> Player -> Bool
isCheck board player =
    let
        kingField =
            Matrix.flatten board
                |> List.foldl
                    (\f field ->
                        case f.figure of
                            Nothing ->
                                field

                            Just figure ->
                                if
                                    ((figure.figure == King)
                                        && (figure.color == player)
                                    )
                                then
                                    f
                                else
                                    field
                    )
                    dummyField
    in
        Matrix.flatten board
            |> List.foldl
                (\f check ->
                    if check then
                        True
                    else
                        case f.figure of
                            Nothing ->
                                False

                            Just figure ->
                                if figure.color == player then
                                    False
                                else if moveIsPossible board f kingField figure then
                                    True
                                else
                                    False
                )
                False


isCheckMate : Matrix Field -> Bool
isCheckMate board =
    let
        boardString =
            boardToString board
    in
        not
            (String.contains "wk" boardString
                && String.contains "bk" boardString
            )


moveIsPossible : Matrix Field -> Field -> Field -> Figure -> Bool
moveIsPossible board selectedField targetField figure =
    case figure.figure of
        Pawn ->
            isPawnMove board selectedField targetField figure.color

        Knight ->
            isKnightMove selectedField targetField

        Bishop ->
            isBishopMove board selectedField targetField

        Rook ->
            isRookMove board selectedField targetField

        King ->
            isKingMove selectedField targetField

        Queen ->
            isQueenMove board selectedField targetField


isMoveLegit : Matrix Field -> Maybe Field -> Field -> Bool
isMoveLegit board selected targetField =
    let
        updatedBoard =
            case selected of
                Nothing ->
                    board

                Just playerField ->
                    Matrix.map
                        (\f ->
                            if f.loc == targetField.loc then
                                { f | figure = playerField.figure }
                            else if f.loc == playerField.loc then
                                { f | figure = Nothing }
                            else
                                f
                        )
                        board
    in
        case selected of
            Nothing ->
                False

            Just selectedField ->
                case selectedField.figure of
                    Nothing ->
                        False

                    Just selectedFigure ->
                        case targetField.figure of
                            Nothing ->
                                ((moveIsPossible board selectedField targetField selectedFigure)
                                    || (selectedFigure.figure
                                            == King
                                            && (isCastlingMove board selectedField targetField)
                                       )
                                )
                                    && (not (isCheck updatedBoard selectedFigure.color))

                            Just targetFigure ->
                                if targetFigure.color == selectedFigure.color then
                                    False
                                else
                                    (moveIsPossible board selectedField targetField selectedFigure)
                                        && (not (isCheck updatedBoard selectedFigure.color))


isPawnMove : Matrix Field -> Field -> Field -> Player -> Bool
isPawnMove board selectedField targetField player =
    -- Todo: En passant
    -- https://en.wikipedia.org/wiki/En_passant
    let
        xDiff =
            (Matrix.col targetField.loc) - (Matrix.col selectedField.loc)

        yDiff =
            (row targetField.loc) - (row selectedField.loc)
    in
        if
            (player
                == Black
                && (yDiff
                        == 1
                        || (((Matrix.row selectedField.loc) == 1 && yDiff == 2)
                                && not
                                    (hasFigure <|
                                        get
                                            (loc
                                                ((row selectedField.loc) + 1)
                                                (Matrix.col selectedField.loc)
                                            )
                                            board
                                    )
                           )
                   )
                && ((xDiff == 0 && (not (hasFigure (Just targetField))))
                        || (hasEnemyFigure targetField player
                                && (abs yDiff)
                                == 1
                                && (abs xDiff)
                                == 1
                           )
                   )
            )
        then
            True
        else if
            (player
                == White
                && ((yDiff
                        == -1
                        || (((Matrix.row selectedField.loc) == 6 && yDiff == -2))
                        && not
                            (hasFigure <|
                                get
                                    (loc
                                        ((row selectedField.loc) - 1)
                                        (Matrix.col selectedField.loc)
                                    )
                                    board
                            )
                    )
                   )
                && ((xDiff == 0 && (not (hasFigure (Just targetField))))
                        || (hasEnemyFigure targetField player
                                && (abs yDiff)
                                == 1
                                && (abs xDiff)
                                == 1
                           )
                   )
            )
        then
            True
        else
            False


isKnightMove : Field -> Field -> Bool
isKnightMove selectedField targetField =
    let
        xDiff =
            (Matrix.col targetField.loc) - (Matrix.col selectedField.loc)

        yDiff =
            (row targetField.loc) - (row selectedField.loc)
    in
        if (abs xDiff) == 2 && (abs yDiff) == 1 then
            True
        else if (abs xDiff) == 1 && (abs yDiff) == 2 then
            True
        else
            False


isBishopMove : Matrix Field -> Field -> Field -> Bool
isBishopMove board selectedField targetField =
    let
        xDiff =
            (Matrix.col targetField.loc) - (Matrix.col selectedField.loc)

        yDiff =
            (row targetField.loc) - (row selectedField.loc)
    in
        if (abs xDiff) == (abs yDiff) then
            nothingBetweenFieldsDiagonal board selectedField targetField
        else
            False


isRookMove : Matrix Field -> Field -> Field -> Bool
isRookMove board selectedField targetField =
    let
        xDiff =
            (Matrix.col targetField.loc) - (Matrix.col selectedField.loc)

        yDiff =
            (row targetField.loc) - (row selectedField.loc)
    in
        if (xDiff == 0 || yDiff == 0) then
            nothingBetweenFieldsCross board selectedField targetField
        else
            False


isKingMove : Field -> Field -> Bool
isKingMove selectedField targetField =
    let
        xDiff =
            (Matrix.col targetField.loc) - (Matrix.col selectedField.loc)

        yDiff =
            (row targetField.loc) - (row selectedField.loc)
    in
        if (abs xDiff) <= 1 && (abs yDiff) <= 1 && not (xDiff == 0 && yDiff == 0) then
            True
        else
            False


isQueenMove : Matrix Field -> Field -> Field -> Bool
isQueenMove board selectedField targetField =
    isBishopMove board selectedField targetField
        || isRookMove board selectedField targetField


isCastlingMove : Matrix Field -> Field -> Field -> Bool
isCastlingMove board selectedField targetField =
    -- Todo: Castling Rights
    let
        player =
            case selectedField.figure of
                Nothing ->
                    White

                Just figure ->
                    figure.color

        leftRookLocation =
            if player == White then
                (loc 7 0)
            else
                (loc 0 0)

        rightRookLocation =
            if player == White then
                (loc 7 7)
            else
                (loc 0 7)

        leftRookField =
            case (Matrix.get leftRookLocation board) of
                Nothing ->
                    dummyField

                Just field ->
                    field

        leftCastlingFieldsEmpty =
            if player == White then
                not
                    ((hasFigure (Matrix.get (loc 7 2) board))
                        || (hasFigure (Matrix.get (loc 7 3) board))
                        || (hasFigure (Matrix.get (loc 7 1) board))
                    )
            else
                not
                    ((hasFigure (Matrix.get (loc 0 2) board))
                        || (hasFigure (Matrix.get (loc 0 3) board))
                        || (hasFigure (Matrix.get (loc 0 1) board))
                    )

        leftCastlingFieldsNotAttacked =
            if player == White then
                not
                    ((isAttackedByEnemy board (loc 7 2) White)
                        || (isAttackedByEnemy board (loc 7 3) White)
                    )
            else
                not
                    ((isAttackedByEnemy board (loc 0 2) White)
                        || (isAttackedByEnemy board (loc 0 3) White)
                    )

        rightRookField =
            case (Matrix.get rightRookLocation board) of
                Nothing ->
                    dummyField

                Just field ->
                    field

        rightCastlingFieldsEmpty =
            if player == White then
                not
                    ((hasFigure (Matrix.get (loc 7 5) board))
                        || (hasFigure (Matrix.get (loc 7 6) board))
                    )
            else
                not
                    ((hasFigure (Matrix.get (loc 0 5) board))
                        || (hasFigure (Matrix.get (loc 0 6) board))
                    )

        rightCastlingFieldsNotAttacked =
            if player == White then
                not
                    ((isAttackedByEnemy board (loc 7 5) White)
                        || (isAttackedByEnemy board (loc 7 6) White)
                    )
            else
                not
                    ((isAttackedByEnemy board (loc 0 5) Black)
                        || (isAttackedByEnemy board (loc 0 6) Black)
                    )

        xDiff =
            (Matrix.col targetField.loc) - (Matrix.col selectedField.loc)

        yDiff =
            (row targetField.loc) - (row selectedField.loc)
    in
        if
            (yDiff
                == 0
                -- Startposition
                && ((player == White && (row selectedField.loc) == 7)
                        || (player == Black && (row selectedField.loc) == 0)
                   )
                -- Cannot be Check
                && (not (isCheck board player))
                && (-- Kingside
                    (xDiff
                        == -2
                        -- Rook needs to be correct position
                        && (hasSpecificFigure leftRookField player Rook)
                        -- Fields in between need to be empty
                        && leftCastlingFieldsEmpty
                        -- Fields in between cannot be attacked
                        && leftCastlingFieldsNotAttacked
                    )
                        || -- Queenside
                           (xDiff
                                == 2
                                -- Rook needs to be correct position
                                && (hasSpecificFigure rightRookField player Rook)
                                -- Fields in between need to be empty
                                && rightCastlingFieldsEmpty
                                -- Fields in between cannot be attacked
                                && rightCastlingFieldsNotAttacked
                           )
                   )
            )
        then
            True
        else
            False


castlingPosition : Matrix Field -> Field -> CastlingPosition
castlingPosition board targetField =
    let
        row =
            Matrix.row targetField.loc

        col =
            Matrix.col targetField.loc
    in
        if row == 0 then
            if col == 2 then
                TopLeft
            else
                TopRight
        else if col == 2 then
            BottomLeft
        else
            BottomRight



-- Helper


dummyField : Field
dummyField =
    { loc = (loc -1 -1), color = getFieldColor (loc -1 -1), figure = Nothing }


hasFigure : Maybe Field -> Bool
hasFigure maybeField =
    case maybeField of
        Nothing ->
            False

        Just field ->
            case field.figure of
                Nothing ->
                    False

                _ ->
                    True


hasEnemyFigure : Field -> Player -> Bool
hasEnemyFigure field player =
    case field.figure of
        Nothing ->
            False

        Just figure ->
            if figure.color == player then
                False
            else
                True


hasSpecificFigure : Field -> Player -> ChessFigure -> Bool
hasSpecificFigure field color chessFigure =
    case field.figure of
        Nothing ->
            False

        Just figure ->
            if figure.color == color && figure.figure == chessFigure then
                True
            else
                False


isAttackedByEnemy : Matrix Field -> Location -> Player -> Bool
isAttackedByEnemy board location player =
    Matrix.flatten board
        |> List.foldl
            (\f isAttacked ->
                if isAttacked then
                    True
                else
                    hasEnemyFigure f player
                        && (moveIsPossible board
                                f
                                (case (Matrix.get location board) of
                                    Nothing ->
                                        dummyField

                                    Just field ->
                                        field
                                )
                                (case f.figure of
                                    Nothing ->
                                        -- dummy pawn, same color
                                        { figure = Pawn, color = player }

                                    Just figure ->
                                        figure
                                )
                           )
            )
            False


nothingBetweenFieldsCross : Matrix Field -> Field -> Field -> Bool
nothingBetweenFieldsCross board selectedField targetField =
    let
        xDiff =
            (Matrix.col targetField.loc) - (Matrix.col selectedField.loc)

        yDiff =
            (row targetField.loc) - (row selectedField.loc)
    in
        Matrix.flatten board
            |> List.foldl
                (\f empty ->
                    let
                        xDiffBetween =
                            (Matrix.col f.loc) - (Matrix.col selectedField.loc)

                        yDiffBetween =
                            (row f.loc) - (row selectedField.loc)
                    in
                        if empty == False then
                            False
                        else if
                            -- Is Rook Move
                            ((xDiffBetween == 0 || yDiffBetween == 0)
                                -- Is the Same Rook Move
                                && ((((xDiffBetween
                                        > 0
                                      )
                                        == (xDiff > 0)
                                     )
                                        && xDiffBetween
                                        /= 0
                                        && xDiff
                                        /= 0
                                    )
                                        || (((yDiffBetween > 0)
                                                == (yDiff > 0)
                                            )
                                                && yDiffBetween
                                                /= 0
                                                && yDiff
                                                /= 0
                                           )
                                   )
                                -- Is Max to Move
                                && ((abs xDiff)
                                        > (abs xDiffBetween)
                                        || (abs yDiff)
                                        > (abs yDiffBetween)
                                   )
                            )
                        then
                            case f.figure of
                                Nothing ->
                                    True

                                Just figure ->
                                    False
                        else
                            True
                )
                True


nothingBetweenFieldsDiagonal : Matrix Field -> Field -> Field -> Bool
nothingBetweenFieldsDiagonal board selectedField targetField =
    let
        xDiff =
            (Matrix.col targetField.loc) - (Matrix.col selectedField.loc)

        yDiff =
            (row targetField.loc) - (row selectedField.loc)
    in
        Matrix.flatten board
            |> List.foldl
                (\f empty ->
                    let
                        xDiffBetween =
                            (Matrix.col f.loc) - (Matrix.col selectedField.loc)

                        yDiffBetween =
                            (row f.loc) - (row selectedField.loc)
                    in
                        if empty == False then
                            False
                        else if
                            -- Is Bishop Move
                            ((abs xDiffBetween)
                                == (abs yDiffBetween)
                                -- Is the Same Move
                                && (((xDiffBetween > 0) == (xDiff > 0)) && xDiffBetween /= 0)
                                && (((yDiffBetween > 0) == (yDiff > 0)) && yDiffBetween /= 0)
                                -- Is Max to Move
                                && ((abs xDiff) > (abs xDiffBetween))
                            )
                        then
                            case f.figure of
                                Nothing ->
                                    True

                                Just figure ->
                                    False
                        else
                            True
                )
                True
