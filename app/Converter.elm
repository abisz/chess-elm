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

stringToPlayer : String -> Player
stringToPlayer string =
    case string of
        "w" ->
            White
        "b" ->
            Black
        _ ->
            White


isValidRowString : String -> Bool
isValidRowString string =
    if List.member string [ "a", "b", "c", "d", "e", "f", "g", "h" ] then
        True
    else
        False


isValidColString : String -> Bool
isValidColString string =
    if List.member string [ "1", "2", "3", "4", "5", "6", "7", "8" ] then
        True
    else
        False


isValidPlayerString : String -> Bool
isValidPlayerString string =
    if List.member string [ "b", "w" ] then
        True
    else
        False


isValidFigureString : String -> Bool
isValidFigureString string =
    if List.member string [ "p", "k", "r", "b", "q", "K" ] then
        True
    else
        False


stringToField : String -> Maybe Field
stringToField string =
    let
        rowString =
            String.slice 2 3 string

        validRow =
            isValidRowString rowString

        row =
            stringToRow rowString

        colString =
            String.slice 3 4 string

        validCol =
            isValidColString colString

        col =
            stringToCol colString

        location =
            (loc row col)

        playerString =
            String.slice 0 1 string

        validPlayer =
            isValidPlayerString playerString

        player =
            stringToPlayer playerString

        figureString =
            String.slice 1 2 string

        validFigure =
            isValidFigureString figureString

        figure =
            { figure = stringToFigure <| figureString
            , color = player
            }
    in
        if validRow && validCol && validPlayer && validFigure then
            Just
                { loc = location
                , color = getFieldColor location
                , figure = Just figure
                }
        else
            Nothing


stringToBoard : String -> Matrix Field
stringToBoard string =
    let
        figureFields =
            String.split ";" string
                |> List.foldl
                    (\fstr fields ->
                        case (stringToField fstr) of
                            Nothing ->
                                fields

                            Just parsedField ->
                                fields ++ [ parsedField ]
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
                            }

                        Just field ->
                            field
            )
