module ChessSocket exposing (..)

import Types exposing (..)
import Regex exposing (..)
import WebSocket
import Converter exposing (locationString, fenToGame)
import Matrix exposing (..)
import BoardGenerator exposing (boardFromFen)


echoServer : String
echoServer =
    "ws://localhost:3000/socket.io/?EIO=3&transport=websocket"


messageRegex : Regex
messageRegex =
    regex "\"(.*)\",{(.*)}"


fenRegex : Regex
fenRegex =
    regex "^update\"fen\":\"(.+)\"$"


encodeMessage : String -> String -> String
encodeMessage code data =
    "42[\"" ++ code ++ "\", \"" ++ data ++ "\"]"


socketInit : Sub Msg
socketInit =
    WebSocket.listen echoServer NewMessage


initConnection : Cmd Msg
initConnection =
    WebSocket.send echoServer "connection"


sendMessage : String -> Cmd Msg
sendMessage message =
    WebSocket.send echoServer message


sendMove : Matrix Field -> Field -> Field -> Cmd Msg
sendMove board selectedField targetField =
    sendMessage
        (encodeMessage
            "move"
            ((locationString selectedField.loc) ++ (locationString targetField.loc))
        )


sendUpdateRequest : Cmd Msg
sendUpdateRequest =
    WebSocket.send echoServer (encodeMessage "getBoard" "")


socketUpdate : Model -> String -> Model
socketUpdate model fenRaw =
    let
        fenString =
            unwrapFEN fenRaw

        newModel =
            fenToGame fenString model
    in
        { newModel
            | message = fenString
            , networkGame = fenString
        }


unwrapFEN : String -> String
unwrapFEN raw =
    let
        matches =
            find All fenRegex raw

        fen =
            List.foldl
                (\match fen ->
                    decodeSubmatchFEN match
                )
                ""
                matches
    in
        fen


decodeSubmatchFEN : Match -> String
decodeSubmatchFEN match =
    List.foldl
        (\submatch result ->
            case submatch of
                Nothing ->
                    result

                Just string ->
                    result ++ string
        )
        ""
        match.submatches


decodeMessage : String -> SocketMessage
decodeMessage rawString =
    let
        code =
            String.left 2 rawString

        json =
            String.dropLeft 2 rawString

        matches =
            find All messageRegex json
    in
        if code /= "42" then
            Error ("Code is not 42: " ++ rawString)
        else
            decodeMatches matches


decodeMatches : List Match -> SocketMessage
decodeMatches matches =
    List.foldl
        (\match matchType ->
            decodeSubmatches match.submatches
        )
        (Error "Decoding Matches")
        matches


decodeSubmatches : List (Maybe String) -> SocketMessage
decodeSubmatches submatches =
    let
        messageType =
            List.foldl
                (\submatch matchType ->
                    if not (String.isEmpty matchType) then
                        matchType
                    else
                        case submatch of
                            Nothing ->
                                ""

                            Just string ->
                                string
                )
                ""
                submatches
    in
        if String.isEmpty messageType then
            Error "No messagetype found"
        else
            case messageType of
                "new connection" ->
                    NewConnection

                "update" ->
                    Update
                        (List.foldl
                            (\sm string ->
                                case sm of
                                    Nothing ->
                                        string

                                    Just text ->
                                        string ++ text
                            )
                            ""
                            submatches
                        )

                _ ->
                    Error "Messagetype not known"
