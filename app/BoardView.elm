module BoardView exposing (drawBoard)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Style exposing (..)
import Matrix exposing (..)
import Move exposing (isMoveLegit)


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


fieldIsSelected : Field -> Selection -> Bool
fieldIsSelected field selection =
    case selection of
        None ->
            False

        Active f ->
            if f.loc == field.loc then
                True
            else
                False


drawBoard : Matrix Field -> Selection -> Html Msg
drawBoard board selection =
    Matrix.map
        (\field ->
            div
                [ (Style.fieldStyles
                    (if fieldIsSelected field selection then
                        Style.selectionColor
                     else if isMoveLegit board selection field then
                        Style.targetColor
                     else
                        field.color
                    )
                  )
                , onClick (ClickField field)
                ]
                [ div [] [ text (drawFigure field) ] ]
        )
        board
        |> flatten
        |> Html.div []
