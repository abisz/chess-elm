module BoardView exposing (drawBoard)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Style exposing (..)
import Matrix exposing (..)


drawFigure : Field -> String
drawFigure field =
    case field.figure of
        Nothing ->
            "_"

        Just figure ->
            case figure.figure of
                Pawn ->
                    if figure.color == Black then
                        "♟"
                    else
                        "♙"

                Knight ->
                    if figure.color == Black then
                        "♞"
                    else
                        "♘"

                Rook ->
                    if figure.color == Black then
                        "♜"
                    else
                        "♖"

                Bishop ->
                    if figure.color == Black then
                        "♝"
                    else
                        "♗"

                Queen ->
                    if figure.color == Black then
                        "♛"
                    else
                        "♕"

                King ->
                    if figure.color == Black then
                        "♚"
                    else
                        "♔"


drawField : Field -> Html Msg
drawField field =
    div
        [ (Style.fieldStyles
            (if field.isSelected then
                Style.selectionColor
             else
                field.color
            )
          )
        , onClick (ClickField field)
        ]
        [ div [] [ text (drawFigure field) ] ]


drawBoard : Matrix Field -> Html Msg
drawBoard board =
    Matrix.map drawField board
        |> flatten
        |> Html.div []
