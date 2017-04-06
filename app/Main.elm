module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Matrix exposing (..)
import Style
import Color exposing (Color)
import Helper exposing (..)
import BoardView exposing (drawBoard)
import Types exposing (..)
import Move exposing (isMoveLegit)
import Converter exposing (boardString)


fieldToText : Field -> String
fieldToText field =
    toString (row field.loc) ++ " / " ++ toString (Matrix.col field.loc)


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


updateBoardSelection : Matrix Field -> Field -> Matrix Field
updateBoardSelection board field =
    Matrix.map
        (\f ->
            if ((f.loc == field.loc) && f.figure /= Nothing) then
                { f | isSelected = True }
            else
                { f | isSelected = False }
        )
        board


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

        board =
            if alreadySelected then
                Matrix.map (\f -> { f | isSelected = False }) model.board
            else
                updateBoardSelection model.board clickedField
    in
        if noFigure then
            model
        else if alreadySelected then
            { model
                | selected = None
                , board = board
            }
        else
            { model
                | selected = (Active clickedField)
                , board = board
            }


changeTurnPlayer : Player -> Player
changeTurnPlayer player =
    if player == Black then
        White
    else
        Black


makeMove : Model -> Field -> Model
makeMove model targetField =
    let
        isSelected =
            case model.selected of
                None ->
                    False

                Active activeField ->
                    True

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
                                { f | figure = Nothing, isSelected = False }
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


view : Model -> Html Msg
view model =
    let
        board =
            drawBoard model.board

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
            , text (boardString model.board)
            ]


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }
