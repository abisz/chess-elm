module Style exposing (..)

import Html
import Html.Attributes exposing (..)
import Color exposing (Color)
import Helper exposing (..)
import CssBasics exposing (..)
import Stylesheet exposing (..)


whiteColor =
    Color.white


blackColor =
    Color.black


greyColor =
    Color.grey


selectionColor =
    Color.yellow


targetColor =
    Color.purple


turnLineStyles : Html.Attribute msg
turnLineStyles =
    toStyleAttribute <|
        [ ( "text-align", Str "center" )
        , ( "margin", Multiple " " [ Unit 0.5 Em, Unit 0 Em ] )
        ]


headingStyles : Html.Attribute msg
headingStyles =
    toStyleAttribute <|
        [ ( "font-size", Unit 2 Em )
        , ( "margin", Multiple " " [ Unit 0.25 Em, Unit 0 Em ] )
        , ( "text-align", Str "center" )
        , ( "color", Col blackColor )
        ]


fieldStyles : Color -> Html.Attribute msg
fieldStyles color =
    Html.Attributes.style
        [ ( "display", "inline-block" )
        , ( "width", "12%" )
        , ( "height", "12%" )
        , ( "padding", "1em 0" )
        , ( "cursor", "pointer" )
        , ( "text-align", "center" )
        , ( "background-color", (colorToCssString color) )
        ]


boardStyles : Html.Attribute msg
boardStyles =
    Html.Attributes.style
        [ ( "width", "600px" )
        , ( "height", "600px" )
        , ( "margin", "0 auto" )
        ]


basicAnyDeclarations : List Declaration
basicAnyDeclarations =
    [ ( "box-sizing", Str "border-box" )
    , ( "font-family", FontStack [ "Crimson Text", "Times New Roman", "serif" ] )
    ]


basicRuleset : List RuleSet
basicRuleset =
    [ { selectors = [ Any ]
      , declarations = basicAnyDeclarations
      , mediaQuery = Nothing
      }
    ]


basicStyleSheet : Stylesheet
basicStyleSheet =
    newStylesheet
        |> addImport "https://fonts.googleapis.com/css?family=Crimson+Text"
        |> withRules basicRuleset


basicStyles : Html.Html msg
basicStyles =
    toStyleNode basicStyleSheet
