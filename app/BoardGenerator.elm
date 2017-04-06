module BoardGenerator exposing (startBoard, getFieldColor)

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
            , isSelected = False
            }
        )
