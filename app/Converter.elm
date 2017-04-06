module Converter exposing (boardString, stringToBoard)

import Types exposing (..)
import Matrix exposing (..)
import BoardGenerator exposing (getFieldColor)


rowString : Int -> String
rowString row =
    case row of
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


colString : Int -> String
colString col =
    case col of
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
        fig ++ row ++ col


boardString : Matrix Field -> String
boardString board =
    Matrix.flatten board
        |> List.foldl
            (\f string ->
                case f.figure of
                    Nothing ->
                        string

                    Just figure ->
                        string ++ (fieldString f) ++ ";"
            )
            ""


stringToRow : String -> Int
stringToRow string =
    case string of
        "a" ->
            0

        "b" ->
            1

        "c" ->
            2

        "d" ->
            3

        "e" ->
            4

        "f" ->
            5

        "g" ->
            6

        "h" ->
            7

        _ ->
            -1


stringToCol : String -> Int
stringToCol string =
    case string of
        "8" ->
            0

        "7" ->
            1

        "6" ->
            2

        "5" ->
            3

        "4" ->
            4

        "3" ->
            5

        "2" ->
            6

        "1" ->
            7

        _ ->
            -1


stringToFigure : String -> ChessFigure
stringToFigure string =
    case string of
        "p" ->
            Pawn

        "k" ->
            Knight

        "b" ->
            Bishop

        "r" ->
            Rook

        "q" ->
            Queen

        "K" ->
            King

        _ ->
            Pawn


stringToField : String -> Field
stringToField string =
    let
        row =
            stringToRow <|
                String.slice 2 3 string

        col =
            stringToCol <|
                String.slice 3 4 string

        location =
            (loc row col)

        player =
            case String.slice 0 1 string of
                "b" ->
                    Black

                "w" ->
                    White

                _ ->
                    White

        figure =
            { figure = stringToFigure <| String.slice 1 2 string
            , color = player
            }
    in
        { loc = location
        , color = getFieldColor location
        , figure = Just figure
        , isSelected = False
        }


stringToBoard : String -> Matrix Field
stringToBoard string =
    let
        figureFields =
            String.split ";" string
                |> List.foldl
                    (\fstr fields ->
                        fields ++ [ (stringToField fstr) ]
                    )
                    []
    in
        square 8
            (\l ->
                let
                    match =
                        List.head <|
                            List.filter
                                (\figureField ->
                                    figureField.loc == l
                                )
                                figureFields
                in
                    case match of
                        Nothing ->
                            { color = (getFieldColor l)
                            , loc = l
                            , figure = Nothing
                            , isSelected = False
                            }

                        Just field ->
                            field
            )
