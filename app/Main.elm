module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Color exposing (Color)
import Matrix exposing (..)
import Style exposing (..)


type alias Model =
    { restart : Bool }


type alias Figure =
    { class : String }


type alias Field =
    { figure : Figure }


model : Model
model =
    { restart = False }


type Msg
    = Restart


update : Msg -> Model -> Model
update msg model =
    case msg of
        Restart ->
            { model | restart = True }


view : Model -> Html Msg
view model =
    div []
        [ h1 [ style [ ( "color", colorToCssString blackColor ) ] ] [ text ("Elm Chess") ]
        , button [ onClick Restart ] [ text "Restart" ]
        , div [] []
        ]


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


colorToCssString : Color -> String
colorToCssString color =
    let
        components =
            Color.toRgb color
    in
        String.concat
            [ "rgb("
            , toString components.red
            , ", "
            , toString components.green
            , ", "
            , toString components.blue
            , ")"
            ]
