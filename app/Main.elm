module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Matrix exposing (..)
import Style
import Color exposing (Color)
import Helper exposing (..)


type alias Figure =
    { class : String
    }


type alias Field =
    { loc : Location
    , color : Color
    , figure : Figure
    , isSelected : Bool
    }


type Selection
    = None
    | Active Field


fieldToText : Field -> String
fieldToText field =
    toString (row field.loc) ++ " / " ++ toString (Matrix.col field.loc)


getFigureColor : Location -> Color
getFigureColor loc =
    if (((row loc) % 2) == ((Matrix.col loc) % 2)) then
        Style.blackColor
    else
        Style.whiteColor


type alias Model =
    { restart : Bool
    , board : Matrix Field
    , selected : Selection
    }


model : Model
model =
    { restart = False
    , board =
        square 8 <|
            (\l -> { color = (getFigureColor l), loc = l, figure = { class = "_" }, isSelected = False })
    , selected = None
    }


selectField : Matrix Field -> Field -> Matrix Field
selectField board field =
    (Matrix.map
        (\f ->
            if (f.loc == field.loc) then
                { f | isSelected = True }
            else
                { f | isSelected = False }
        )
        board
    )


type Msg
    = Restart
    | SelectField Field


update : Msg -> Model -> Model
update msg model =
    case msg of
        Restart ->
            { model | restart = True }

        SelectField field ->
            { model
                | selected = (Active field)
                , board = (selectField model.board field)
            }


drawField : Field -> Html Msg
drawField field =
    Html.div
        [ (Style.fieldStyles
            (if field.isSelected then
                Style.selectionColor
             else
                field.color
            )
          )
        , onClick (SelectField field)
        ]
        [ text (fieldToText field) ]


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
