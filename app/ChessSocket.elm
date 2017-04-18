module ChessSocket exposing (..)

import Types exposing (..)
import Regex exposing (..)
import WebSocket


echoServer : String
echoServer =
    "ws://localhost:3000/socket.io/?EIO=3&transport=websocket"


messageRegex : Regex
messageRegex =
    regex "\"(.*)\",{(.*)}"


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



--[\"new connection\",{\"id\":\"UA--Ww5XMyC7_E_TAAAA\"}]
--[{
--    match = "\"new connection\",{\"id\":\"UA--Ww5XMyC7_E_TAAAA\"}"
--    , submatches = [
--        Just "new connection"
--        ,Just "\"id\":\"UA--Ww5XMyC7_E_TAAAA\""
--    ]
--    , index = 1
--    , number = 1
--}]


decodeMatches : List Match -> SocketMessage
decodeMatches matches =
    List.foldl
        (\match matchType ->
            --            case matchType of
            --                Error errorMessage ->
            --                    Error errorMessage
            --
            --                _ ->
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

                _ ->
                    Error "Messagetype not known"



--        Error "match error"
--    let
--        code =
--                String.left 2 string
--
--            json =
--                String.dropLeft 2 string
--
--            matches =
--                find All messageRegex json
--
--            messageType =
--                List.foldl (\m msgType ->
--                    let
--                        typeString =
--                            List.foldl (\subm submType->
--                                if not (String.isEmpty submType) then
--                                    case subm of
--                                        Nothing ->
--                                            submType
--                                        Just string ->
--                                            string
--                                else
--                                    submType
--                            ) "" m.submatches
--
--                    in
--                        case typeString of
--                            "new connection" ->
--                                NewConnection
--                            _ ->
--                                Error
--                ) Error matches
--
--            message =
--                case messageType of
--                    Error ->
--                        "Error"
--                    NewConnection ->
--                        "New Connection"
--        if code == "42" then
--            { model | message = message }
--        else
--            { model | message = "Not code 42: " ++ string }
