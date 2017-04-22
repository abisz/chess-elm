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


type alias Model =
    { board : Matrix Field
    , localGame : String
    , networkGame : String
    , roomInput : String
    , room : String
    , selected : Maybe Field
    , turn : Player
    , checkMate : Bool
    , message : String
    , mode : GameMode
    }


type GameMode
    = Local
    | Network


type Msg
    = ClickField Field
    | RenderBoard String
    | NewMessage String
    | SendMessage String
    | ChangeGameMode GameMode
    | RoomInput String
    | ConnectRoom


type SocketMessage
    = Error String
    | NewConnection
    | Update String


type CastlingPosition
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
