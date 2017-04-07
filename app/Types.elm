module Types exposing (..)

import Matrix exposing (..)
import Color exposing (..)


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
    }


type Selection
    = None
    | Active Field


type alias Model =
    { board : Matrix Field
    , selected : Selection
    , turn : Player
    }


type Msg
    = ClickField Field
    | RenderBoard String
