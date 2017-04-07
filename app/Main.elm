module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Matrix exposing (..)
import Style
import Helper exposing (..)
import BoardView exposing (drawBoard)
import Types exposing (..)
import Move exposing (isMoveLegit)
import Converter exposing (boardToString, stringToBoard)
import BoardGenerator exposing (startBoard)


fieldToText : Field -> String
fieldToText field =
    toString (row field.loc) ++ " / " ++ toString (Matrix.col field.loc)


model : Model
model =
    { board = startBoard
    , selected = None
    , turn = Black
    }


fieldAlreadySelected : Selection -> Field -> Bool
fieldAlreadySelected selected field =
    case selected of
        None ->
            False

        Active f ->
            if f.loc == field.loc then
                True
            else
                False


selectField : Model -> Field -> Model
selectField model clickedField =
    let
        noFigure =
            if clickedField.figure == Nothing then
                True
            else
                False

        alreadySelected =
            fieldAlreadySelected model.selected clickedField
    in
        if noFigure then
            model
        else if alreadySelected then
            { model | selected = None }
        else
            { model | selected = (Active clickedField) }


changeTurnPlayer : Player -> Player
changeTurnPlayer player =
    if player == Black then
        White
    else
        Black


makeMove : Model -> Field -> Model
makeMove model targetField =
    let
        legitMove =
            isMoveLegit model.board model.selected targetField

        updatedBoard =
            case model.selected of
                None ->
                    model.board

                Active playerField ->
                    Matrix.map
                        (\f ->
                            if f.loc == targetField.loc then
                                { f | figure = playerField.figure }
                            else if f.loc == playerField.loc then
                                { f | figure = Nothing }
                            else
                                f
                        )
                        model.board

        changeTurn =
            case model.selected of
                None ->
                    False

                Active field ->
                    True
    in
        if legitMove then
            { model
                | selected = None
                , board = updatedBoard
                , turn =
                    (if changeTurn then
                        changeTurnPlayer model.turn
                     else
                        model.turn
                    )
            }
        else
            model


clickField : Model -> Field -> Model
clickField model clickedField =
    let
        legitSelection =
            case model.selected of
                -- pick up
                None ->
                    case clickedField.figure of
                        Nothing ->
                            False

                        Just figure ->
                            if figure.color == model.turn then
                                True
                            else
                                False

                -- put down
                Active field ->
                    case clickedField.figure of
                        Nothing ->
                            False

                        Just figure ->
                            case field.figure of
                                Nothing ->
                                    False

                                Just figurePlayer ->
                                    figure.color == figurePlayer.color
    in
        if legitSelection then
            selectField model clickedField
        else
            makeMove model clickedField


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickField field ->
            clickField model field

        RenderBoard string ->
            { model | board = (stringToBoard (String.trim string)), selected = None }


view : Model -> Html Msg
view model =
    let
        board =
            drawBoard model.board model.selected

        boardString =
            boardToString model.board

        selection =
            (case model.selected of
                None ->
                    "No Selection"

                Active field ->
                    fieldToText field
            )
    in
        div []
            [ h1 [ style [ ( "color", colorToCssString Style.blackColor ) ] ] [ text ("Elm Chess") ]
            , text ("Selected: " ++ selection)
            , text
                ("Turn: "
                    ++ (if model.turn == Black then
                            "Black"
                        else
                            "White"
                       )
                )
            , board
            , textarea
                [ style
                    [ ( "width", "400px" )
                    , ( "height", "150px" )
                    ]
                , value boardString
                , onInput RenderBoard
                ]
                []
            ]


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }
