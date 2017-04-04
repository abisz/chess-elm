module Style exposing (..)

import Html
import Html.Attributes exposing (..)
import Color exposing (Color)
import Helper exposing (..)


whiteColor =
    Color.white


blackColor =
    Color.black


fieldStyles : Color -> Html.Attribute msg
fieldStyles color =
    Html.Attributes.style
        [ ( "display", "inline-block" )
        , ( "width", "12%" )
        , ( "padding", "1em 0" )
        , ( "cursor", "pointer" )
        , ( "text-align", "center" )
        , ( "background-color", (colorToCssString color) )
        , ( "color"
          , (let
                textColor =
                    if (color == whiteColor) then
                        blackColor
                    else
                        whiteColor
             in
                colorToCssString textColor
            )
          )
        ]
