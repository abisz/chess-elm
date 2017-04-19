module Converter exposing (boardToString, fieldString)

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
            case figure.figure of
                Pawn ->
                    "p"

                Knight ->
                    "k"

                Bishop ->
                    "b"

                Rook ->
                    "r"

                Queen ->
                    "q"

                King ->
                    "K"
    in
        color ++ fig


fieldString : Field -> String
fieldString field =
    let
        fig =
            case field.figure of
                Nothing ->
                    ""

                Just figure ->
                    figureString figure

        row =
            rowString (Matrix.row field.loc)

        col =
            colString (Matrix.col field.loc)
    in
        fig ++ row ++ col ++ ";"


boardToString : Matrix Field -> String
boardToString board =
    Matrix.flatten board
        |> List.foldl
            (\f string ->
                case f.figure of
                    Nothing ->
                        string

                    Just figure ->
                        string ++ (fieldString f)
            )
            ""
