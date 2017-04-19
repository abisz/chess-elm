module BoardView exposing (drawBoard)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import CssBasics exposing (..)
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


fieldIsSelected : Field -> Maybe Field -> Bool
fieldIsSelected field selection =
    case selection of
        Nothing ->
            False

        Just f ->
            if f.loc == field.loc then
                True
            else
                False


drawBoard : Matrix Field -> Maybe Field -> Html Msg
drawBoard board selection =
    Matrix.map
        (\field ->
            div
                [ attribute "class" "field"
                , style
                    [ ( "background-color"
                      , encodeCssValue <|
                            if fieldIsSelected field selection then
                                Col Style.selectionColor
                            else if isMoveLegit board selection field then
                                Col Style.targetColor
                            else
                                Col field.color
                      )
                    ]
                , onClick (ClickField field)
                ]
                [ div [] [ text (drawFigure field) ] ]
        )
        board
        |> flatten
        |> Html.div [ Style.boardStyles ]
