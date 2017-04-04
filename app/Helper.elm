module Helper exposing (colorToCssString)

import Color exposing (Color)


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
