module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Matrix exposing (..)
import Style
import BoardView exposing (drawBoard)
import Types exposing (..)
import Move exposing (isMoveLegit, isCheckMate, isCastlingMove, castlingPosition)
import Converter exposing (boardToString, gameToFen, fenToGame)
import BoardGenerator exposing (startBoard, boardFromString)
import ChessSocket exposing (..)


fieldAlreadySelected : Maybe Field -> Field -> Bool
fieldAlreadySelected selected field =
    case selected of
        Nothing ->
            False

        Just f ->
            if f.loc == field.loc then
                True
            else
                False


selectField : Model -> Field -> Model
selectField model clickedField =
    let
        noFigure =
            if clickedField.figure == Nothing then
                True
            else
                False

        alreadySelected =
            fieldAlreadySelected model.selected clickedField
    in
        if noFigure then
            model
        else if alreadySelected then
            { model | selected = Nothing }
        else
            { model | selected = (Just clickedField) }


changeTurnPlayer : Player -> Player
changeTurnPlayer player =
    if player == Black then
        White
    else
        Black


makeMove : Model -> Field -> ( Model, Cmd Msg )
makeMove model targetField =
    case model.mode of
        Local ->
            ( makeMoveLocal model targetField, Cmd.none )

        Network ->
            makeMoveNetwork model targetField


makeMoveNetwork : Model -> Field -> ( Model, Cmd Msg )
makeMoveNetwork model targetField =
    case model.selected of
        Nothing ->
            ( model, Cmd.none )

        Just field ->
            ( model, sendMove model.board field targetField )


makeMoveLocal : Model -> Field -> Model
makeMoveLocal model targetField =
    -- Todo: Fifty-Move Rule
    -- https://en.wikipedia.org/wiki/Fifty-move_rule
    -- Todo: Pawn Promotion
    let
        legitMove =
            isMoveLegit model.board model.selected targetField

        isCastling =
            case model.selected of
                Nothing ->
                    False

                Just field ->
                    isCastlingMove model.board field targetField

        castlingPos =
            castlingPosition model.board targetField

        rookCastlingStart =
            case castlingPos of
                TopLeft ->
                    loc 0 0

                TopRight ->
                    loc 0 7

                BottomLeft ->
                    loc 7 0

                BottomRight ->
                    loc 7 7

        rookCastlingTarget =
            case castlingPos of
                TopLeft ->
                    loc 0 3

                TopRight ->
                    loc 0 5

                BottomLeft ->
                    loc 7 3

                BottomRight ->
                    loc 7 5

        rook =
            Figure Rook White

        updatedBoard =
            case model.selected of
                Nothing ->
                    model.board

                Just playerField ->
                    Matrix.map
                        (\f ->
                            if f.loc == targetField.loc then
                                { f | figure = playerField.figure }
                            else if f.loc == playerField.loc then
                                { f | figure = Nothing }
                            else if isCastling then
                                if f.loc == rookCastlingStart then
                                    { f | figure = Nothing }
                                else if f.loc == rookCastlingTarget then
                                    { f | figure = Just rook }
                                else
                                    f
                            else
                                f
                        )
                        model.board

        checkMate =
            isCheckMate updatedBoard

        changeTurn =
            case model.selected of
                Nothing ->
                    False

                Just field ->
                    True

        newModel =
            { model
                | selected = Nothing
                , board = updatedBoard
                , checkMate = checkMate
                , turn =
                    (if changeTurn && not checkMate then
                        changeTurnPlayer model.turn
                     else
                        model.turn
                    )
            }
    in
        if legitMove then
            { newModel | localGame = (gameToFen newModel) }
        else
            model


clickField : Model -> Field -> ( Model, Cmd Msg )
clickField model clickedField =
    let
        legitSelection =
            case model.selected of
                -- pick up
                Nothing ->
                    case clickedField.figure of
                        Nothing ->
                            False

                        Just figure ->
                            if figure.color == model.turn then
                                True
                            else
                                False

                -- put down
                Just field ->
                    case clickedField.figure of
                        Nothing ->
                            False

                        Just figure ->
                            case field.figure of
                                Nothing ->
                                    False

                                Just figurePlayer ->
                                    figure.color == figurePlayer.color
    in
        if model.checkMate then
            ( model, Cmd.none )
        else if legitSelection then
            ( selectField model clickedField, Cmd.none )
        else
            makeMove model clickedField


newMessage : Model -> String -> Model
newMessage model string =
    let
        messageType =
            (decodeMessage string)

        message =
            case messageType of
                Error string ->
                    "Error: " ++ string

                NewConnection ->
                    "New Connection"

                _ ->
                    ""
    in
        case messageType of
            Update fen ->
                if model.mode == Network then
                    socketUpdate model fen
                else
                    { model | message = message }

            _ ->
                { model | message = message }


startPositionFen : String
startPositionFen =
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


init : ( Model, Cmd Msg )
init =
    ( { board = startBoard
      , localGame = startPositionFen
      , networkGame = startPositionFen
      , roomInput = ""
      , room = ""
      , selected = Nothing
      , turn = White
      , checkMate = False
      , message = ""
      , mode = Local
      }
      --    , initConnection
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickField field ->
            clickField model field

        RenderBoard string ->
            ( { model
                | board = (boardFromString (String.trim string))
                , selected = Nothing
              }
            , Cmd.none
            )

        NewMessage string ->
            ( newMessage model string, Cmd.none )

        SendMessage string ->
            ( model, sendMessage string )

        ChangeGameMode mode ->
            let
                cmd =
                    if mode == Network then
                        sendUpdateRequest
                    else
                        Cmd.none

                newModel =
                    case mode of
                        Local ->
                            fenToGame model.localGame model

                        Network ->
                            fenToGame model.networkGame model
            in
                ( { newModel | mode = mode }, cmd )

        RoomInput roomString ->
            ( { model | roomInput = roomString }, Cmd.none )

        ConnectRoom ->
            ( model, connectToRoom model.roomInput )


networkSettingsView : String -> Html Msg
networkSettingsView roomName =
    div [ Style.turnLineStyles ]
        [ h2 [ Style.turnLineStyles ] [ text ("Room:" ++ roomName) ]
        , input [ Style.turnLineStyles, onInput RoomInput ] []
        , button [ onClick ConnectRoom ] [ text "Connect to Room" ]
        ]


view : Model -> Html Msg
view model =
    let
        board =
            drawBoard model.board model.selected

        boardString =
            boardToString model.board

        currentPlayer =
            if model.turn == Black then
                "Black"
            else
                "White"

        turnLine =
            if model.checkMate then
                text
                    ("Checkmate: The Winner is "
                        ++ currentPlayer
                    )
            else
                text
                    ("Turn: "
                        ++ (if model.turn == Black then
                                "Black"
                            else
                                "White"
                           )
                    )
    in
        div []
            [ h1 [ Style.headingStyles ] [ text ("Elm Chess") ]
            , text (model.message)

            --            , text (gameToFen model)
            , div [ class "btnModeContainer" ]
                [ h3 [] [ text "Game Mode:" ]
                , button
                    [ onClick (ChangeGameMode Local)
                    , class
                        (if model.mode == Local then
                            "active btnMode"
                         else
                            "inactive btnMode"
                        )
                    ]
                    [ text ("Local") ]
                , button
                    [ onClick (ChangeGameMode Network)
                    , class
                        (if model.mode == Network then
                            "active btnMode"
                         else
                            "inactive btnMode"
                        )
                    ]
                    [ text ("Network") ]
                ]
            , h2 [ Style.turnLineStyles ]
                [ turnLine ]
            , (if model.mode == Network then
                networkSettingsView model.room
               else
                div [] []
              )
            , board

            --            , textarea
            --                [ style
            --                    [ ( "width", "400px" )
            --                    , ( "height", "150px" )
            --                    ]
            --                , value boardString
            --                , onInput RenderBoard
            --                ]
            --                []
            , Style.basicStyles
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    socketInit


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
