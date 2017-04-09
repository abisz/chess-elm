module BoardGenerator exposing (startBoard, getFieldColor, boardFromString, fieldFromString)

import Types exposing (..)
import Matrix exposing (..)
import Color exposing (Color)
import Style exposing (..)


getFieldColor : Location -> Color
getFieldColor loc =
    if (((row loc) % 2) == ((Matrix.col loc) % 2)) then
        Style.greyColor
    else
        Style.whiteColor


getDefaultFigure : Location -> Maybe Figure
getDefaultFigure loc =
    let
        row =
            Matrix.row loc

        col =
            Matrix.col loc
    in
        if row == 1 then
            Just { figure = Pawn, color = Black }
        else if row == 6 then
            Just { figure = Pawn, color = White }
        else if row == 0 && (col == 0 || col == 7) then
            Just { figure = Rook, color = Black }
        else if row == 7 && (col == 0 || col == 7) then
            Just { figure = Rook, color = White }
        else if row == 0 && (col == 1 || col == 6) then
            Just { figure = Knight, color = Black }
        else if row == 7 && (col == 1 || col == 6) then
            Just { figure = Knight, color = White }
        else if row == 0 && (col == 2 || col == 5) then
            Just { figure = Bishop, color = Black }
        else if row == 7 && (col == 2 || col == 5) then
            Just { figure = Bishop, color = White }
        else if row == 0 && col == 3 then
            Just { figure = Queen, color = Black }
        else if row == 7 && col == 3 then
            Just { figure = Queen, color = White }
        else if row == 0 && col == 4 then
            Just { figure = King, color = Black }
        else if row == 7 && col == 4 then
            Just { figure = King, color = White }
        else
            Nothing


startBoard : Matrix Field
startBoard =
    square 8 <|
        (\l ->
            { color = (getFieldColor l)
            , loc = l
            , figure = (getDefaultFigure l)
            }
        )


boardFromString : String -> Matrix Field
boardFromString string =
    let
        figureFields =
            String.split ";" string
                |> List.foldl
                    (\fstr fields ->
                        case (fieldFromString fstr) of
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


fieldFromString : String -> Maybe Field
fieldFromString string =
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


isValidColString : String -> Bool
isValidColString string =
    if List.member string [ "a", "b", "c", "d", "e", "f", "g", "h" ] then
        True
    else
        False


isValidRowString : String -> Bool
isValidRowString string =
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


stringToCol : String -> Int
stringToCol string =
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


stringToRow : String -> Int
stringToRow string =
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


stringToPlayer : String -> Player
stringToPlayer string =
    case string of
        "w" ->
            White

        "b" ->
            Black

        _ ->
            White
