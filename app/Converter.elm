module Converter exposing (boardToString, locationString, boardToFen)

import Types exposing (..)
import Matrix exposing (..)


colString : Int -> String
colString col =
    case col of
        0 ->
            "a"

        1 ->
            "b"

        2 ->
            "c"

        3 ->
            "d"

        4 ->
            "e"

        5 ->
            "f"

        6 ->
            "g"

        7 ->
            "h"

        _ ->
            "_"


rowString : Int -> String
rowString row =
    case row of
        0 ->
            "8"

        1 ->
            "7"

        2 ->
            "6"

        3 ->
            "5"

        4 ->
            "4"

        5 ->
            "3"

        6 ->
            "2"

        7 ->
            "1"

        _ ->
            "_"


chessFigureString : ChessFigure -> String
chessFigureString figure =
    case figure of
        Pawn ->
            "p"

        Knight ->
            "n"

        Bishop ->
            "b"

        Rook ->
            "r"

        Queen ->
            "q"

        King ->
            "k"


figureString : Figure -> String
figureString figure =
    let
        color =
            case figure.color of
                Black ->
                    "b"

                White ->
                    "w"

        fig =
            chessFigureString figure.figure
    in
        color ++ fig


locationString : Location -> String
locationString loc =
    let
        col =
            colString (Matrix.col loc)

        row =
            rowString (Matrix.row loc)
    in
        col ++ row


getFieldString : Field -> String
getFieldString field =
    let
        fig =
            case field.figure of
                Nothing ->
                    ""

                Just figure ->
                    figureString figure

        location =
            locationString field.loc
    in
        fig ++ location ++ ";"


boardToString : Matrix Field -> String
boardToString board =
    Matrix.flatten board
        |> List.foldl
            (\f string ->
                case f.figure of
                    Nothing ->
                        string

                    Just figure ->
                        string ++ (getFieldString f)
            )
            ""


rowToFen : List Field -> String
rowToFen row =
    List.foldl
        (\field rowString ->
            let
                hasFigure =
                    case field.figure of
                        Nothing ->
                            False

                        Just figure ->
                            True

                player =
                    case field.figure of
                        Nothing ->
                            White

                        Just figure ->
                            figure.color

                figureCase =
                    if player == White then
                        String.toUpper
                    else
                        String.toLower

                fieldString =
                    if hasFigure then
                        figureCase
                            (chessFigureString
                                (case field.figure of
                                    Nothing ->
                                        Pawn

                                    Just figure ->
                                        figure.figure
                                )
                            )
                    else
                        "0"

                lastCharacterInString =
                    String.right 1 rowString

                lastIsInt =
                    case (String.toInt lastCharacterInString) of
                        Ok value ->
                            True

                        Err message ->
                            False

                lastInt =
                    case (String.toInt lastCharacterInString) of
                        Ok value ->
                            value

                        Err message ->
                            -1
            in
                if hasFigure then
                    rowString ++ fieldString
                else if lastIsInt then
                    (String.dropRight 1 rowString) ++ (toString (lastInt + 1))
                else
                    rowString ++ "1"
        )
        ""
        row


boardToFen : Matrix Field -> String
boardToFen board =
    let
        rowList =
            Matrix.toList board

        rowStringList =
            List.map rowToFen rowList
    in
        List.foldl
            (\row board ->
                if String.isEmpty board then
                    row
                else
                    board ++ "/" ++ row
            )
            ""
            rowStringList


moveToSANString : Matrix Field -> Field -> Field -> String
moveToSANString board selectedField targetField =
    -- Todo: Refactor to FEN
    -- https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
    let
        player =
            case selectedField.figure of
                Nothing ->
                    White

                Just figure ->
                    figure.color

        playerCase =
            if player == White then
                String.toUpper
            else
                String.toLower

        figure =
            case selectedField.figure of
                Nothing ->
                    ""

                Just figure ->
                    case figure.figure of
                        Pawn ->
                            ""

                        _ ->
                            chessFigureString figure.figure

        field =
            locationString targetField.loc
    in
        (playerCase figure) ++ field
