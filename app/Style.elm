module Style exposing (..)

import Html
import Html.Attributes exposing (..)
import Color exposing (Color)
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
        , ( "margin", Multiple " " [ Unit 0.5 Em, Unit 0 NoUnit ] )
        ]


headingStyles : Html.Attribute msg
headingStyles =
    toStyleAttribute <|
        [ ( "font-size", Unit 2 Em )
        , ( "margin", Multiple " " [ Unit 0.25 Em, Unit 0 NoUnit ] )
        , ( "text-align", Str "center" )
        , ( "color", Col blackColor )
        ]


boardStyles : Html.Attribute msg
boardStyles =
    Html.Attributes.style
        [ ( "width", "600px" )
        , ( "height", "600px" )
        , ( "margin", "0 auto" )
        ]


basicFieldDeclarations : List Declaration
basicFieldDeclarations =
    [ ( "display", Str "inline-block" )
    , ( "width", Unit 12 Percent )
    , ( "height", Unit 12 Percent )
    , ( "padding", Multiple " " [ Unit 1 Em, Unit 0 NoUnit ] )
    , ( "cursor", Str "pointer" )
    , ( "text-align", Str "center" )
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
    , { selectors = [ Class "field" ]
      , declarations = basicFieldDeclarations
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
