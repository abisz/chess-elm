module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Matrix exposing (..)
import Style
import Color exposing (Color)
import Helper exposing (..)


type ChessFigure
    = Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King


type Player
    = Black
    | White


type alias Figure =
    { figure : ChessFigure
    , color : Player
    }


type alias Field =
    { loc : Location
    , color : Color
    , figure : Maybe Figure
    , isSelected : Bool
    }


type Selection
    = None
    | Active Field


fieldToText : Field -> String
fieldToText field =
    toString (row field.loc) ++ " / " ++ toString (Matrix.col field.loc)


getFieldColor : Location -> Color
getFieldColor loc =
    if (((row loc) % 2) == ((Matrix.col loc) % 2)) then
        Style.greyColor
    else
        Style.whiteColor


type alias Model =
    { restart : Bool
    , board : Matrix Field
    , selected : Selection
    }


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
        else if row == 7 && col == 4 then
            Just { figure = Queen, color = White }
        else if row == 0 && col == 4 then
            Just { figure = King, color = Black }
        else if row == 7 && col == 3 then
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
    { restart = False
    , board = startBoard
    , selected = None
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


moveFigure : Model -> Field -> Model
moveFigure model targetField =
    let
        isSelected =
            case model.selected of
                None ->
                    False

                Active activeField ->
                    True

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
    in
        { model
            | selected = None
            , board = updatedBoard
        }


clickField : Model -> Field -> Model
clickField model clickedField =
    let
        legitSelection =
            if model.selected == None then
                True
            else
                case model.selected of
                    None ->
                        False

                    Active field ->
                        case clickedField.figure of
                            Nothing ->
                                False

                            Just figureTarget ->
                                case field.figure of
                                    Nothing ->
                                        False

                                    Just figurePlayer ->
                                        figureTarget.color == figurePlayer.color
    in
        if legitSelection then
            selectField model clickedField
        else
            moveFigure model clickedField


type Msg
    = Restart
    | ClickField Field
    | SelectField Field


update : Msg -> Model -> Model
update msg model =
    case msg of
        Restart ->
            { model | restart = True }

        SelectField field ->
            selectField model field

        ClickField field ->
            clickField model field


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
            , button [ onClick Restart ] [ text "Restart" ]
            , text ("Selected: " ++ selection)
            , board
            ]


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }
