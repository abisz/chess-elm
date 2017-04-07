module Move exposing (isMoveLegit)

import Types exposing (..)
import Matrix exposing (..)


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


isMoveLegit : Matrix Field -> Selection -> Field -> Bool
isMoveLegit board selected targetField =
    case selected of
        None ->
            False

        Active selectedField ->
            case selectedField.figure of
                Nothing ->
                    False

                Just selectedFigure ->
                    case targetField.figure of
                        Nothing ->
                            moveIsPossible board selectedField targetField selectedFigure

                        Just targetFigure ->
                            if targetFigure.color == selectedFigure.color then
                                False
                            else
                                moveIsPossible board selectedField targetField selectedFigure


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


isPawnMove : Matrix Field -> Field -> Field -> Player -> Bool
isPawnMove board selectedField targetField player =
    -- Todo: Pawn can't beat straight
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
                && (xDiff == 0 || (hasEnemyFigure targetField player && (abs yDiff) <= 1))
            )
        then
            True
        else if
            (player
                == White
                && (yDiff
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
                && (xDiff == 0 || (hasEnemyFigure targetField player && (abs yDiff) <= 1))
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



-- Helper


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
