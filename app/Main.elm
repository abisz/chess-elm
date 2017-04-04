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
    }


type alias Model =
    { restart : Bool
    , board : Matrix Field
    }


getFigureColor : Location -> Color
getFigureColor loc =
    if (((row loc) % 2) == ((Matrix.col loc) % 2)) then
        Style.blackColor
    else
        Style.whiteColor


model : Model
model =
    { restart = False
    , board =
        square 8 <|
            (\l -> { color = (getFigureColor l), loc = l, figure = { class = "_" } })
    }


type Msg
    = Restart


update : Msg -> Model -> Model
update msg model =
    case msg of
        Restart ->
            { model | restart = True }


drawField : Field -> Html div
drawField field =
    Html.div [ (Style.fieldStyles field.color) ]
        [ text (toString (row field.loc) ++ "/" ++ toString (Matrix.col field.loc)) ]


drawBoard : Matrix Field -> Html div
drawBoard board =
    Matrix.map drawField board
        |> flatten
        |> Html.div []


view : Model -> Html Msg
view model =
    let
        board =
            drawBoard model.board
    in
        div []
            [ h1 [ style [ ( "color", colorToCssString Style.blackColor ) ] ] [ text ("Elm Chess") ]
            , button [ onClick Restart ] [ text "Restart" ]
            , board
            ]


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }
