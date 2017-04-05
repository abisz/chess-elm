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
    , isSelected : Bool
    }


type Selection
    = None
    | Active Field


type alias Model =
    { restart : Bool
    , board : Matrix Field
    , selected : Selection
    }


type Msg
    = Restart
    | ClickField Field
    | SelectField Field
