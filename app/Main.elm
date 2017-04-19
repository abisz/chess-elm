module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Matrix exposing (..)
import Style
import BoardView exposing (drawBoard)
import Types exposing (..)
import Move exposing (isMoveLegit, isCheckMate, isCastlingMove, castlingPosition)
import Converter exposing (boardToString, moveToSANString)
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
    if model.solo then
        ( makeMoveSolo model targetField, Cmd.none )
    else
        makeMoveMulti model targetField


makeMoveMulti : Model -> Field -> ( Model, Cmd Msg )
makeMoveMulti model targetField =
    case model.selected of
        Nothing ->
            ( model, Cmd.none )

        Just field ->
            ( model, sendMove model.board field targetField )


makeMoveSolo : Model -> Field -> Model
makeMoveSolo model targetField =
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
    in
        if legitMove then
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
        message =
            case (decodeMessage string) of
                Error string ->
                    "Error: " ++ string

                NewConnection ->
                    "New Connection"
    in
        { model | message = message }


init : ( Model, Cmd Msg )
init =
    ( { board = startBoard
      , selected = Nothing
      , turn = White
      , checkMate = False
      , message = ""
      , solo = False
      }
    , initConnection
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickField field ->
            clickField model field

        RenderBoard string ->
            ( { model | board = (boardFromString (String.trim string)), selected = Nothing }, Cmd.none )

        NewMessage string ->
            ( newMessage model string, Cmd.none )

        SendMessage string ->
            ( model, sendMessage string )


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
            , button [ onClick (SendMessage (encodeMessage "move" "bar")) ] [ text ("Send Move") ]
            , h2 [ Style.turnLineStyles ]
                [ turnLine ]
            , board
            , textarea
                [ style
                    [ ( "width", "400px" )
                    , ( "height", "150px" )
                    ]
                , value boardString
                , onInput RenderBoard
                ]
                []
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
