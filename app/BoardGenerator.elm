module BoardGenerator exposing (startBoard, getFieldColor, boardFromString, fieldFromString, boardFromFen)

import Types exposing (..)
import Matrix exposing (..)
import Color exposing (Color)
import Style exposing (..)


getFieldColor : Location -> Color
getFieldColor loc =
    if (((row loc) % 2) == ((Matrix.col loc) % 2)) then
        Style.greyColor
    else
        Style.whiteColor


startBoard : Matrix Field
startBoard =
    boardFromString "bra8;bnb8;bbc8;bqd8;bke8;bbf8;bng8;brh8;bpa7;bpb7;bpc7;bpd7;bpe7;bpf7;bpg7;bph7;wpa2;wpb2;wpc2;wpd2;wpe2;wpf2;wpg2;wph2;wra1;wnb1;wbc1;wqd1;wke1;wbf1;wng1;wrh1;"


boardFromFen : String -> Matrix Field
boardFromFen string =
    let
        rowStrings =
            String.split "/" string
    in
        square 8
            (\l ->
                let
                    col =
                        Matrix.col l

                    row =
                        Matrix.row l

                    rowString =
                        case
                            (List.drop row rowStrings
                                |> List.head
                            )
                        of
                            Nothing ->
                                "8"

                            Just string ->
                                string

                    rowList =
                        List.foldl
                            (\c list ->
                                case String.toInt c of
                                    Err message ->
                                        list ++ [ c ]

                                    Ok value ->
                                        list ++ (List.repeat value "0")
                            )
                            []
                            (String.split "" rowString)

                    figure =
                        case
                            (List.drop col rowList
                                |> List.head
                            )
                        of
                            Nothing ->
                                Nothing

                            Just char ->
                                case char of
                                    "0" ->
                                        Nothing

                                    _ ->
                                        Just
                                            { figure = stringToFigure (String.toLower char)
                                            , color = characterToPlayer char
                                            }
                in
                    { loc = l, color = (getFieldColor l), figure = figure }
            )


characterToPlayer : String -> Player
characterToPlayer char =
    if char == String.toLower char then
        Black
    else
        White


boardFromString : String -> Matrix Field
boardFromString string =
    let
        figureFields =
            String.split ";" string
                |> List.foldl
                    (\fstr fields ->
                        case (fieldFromString fstr) of
                            Nothing ->
                                fields

                            Just parsedField ->
                                fields ++ [ parsedField ]
                    )
                    []
    in
        square 8
            (\l ->
                let
                    match =
                        List.head <|
                            List.filter
                                (\figureField ->
                                    figureField.loc == l
                                )
                                figureFields
                in
                    case match of
                        Nothing ->
                            { color = (getFieldColor l)
                            , loc = l
                            , figure = Nothing
                            }

                        Just field ->
                            field
            )


fieldFromString : String -> Maybe Field
fieldFromString string =
    let
        colString =
            String.slice 2 3 string

        validCol =
            isValidColString colString

        col =
            stringToCol colString

        rowString =
            String.slice 3 4 string

        validRow =
            isValidRowString rowString

        row =
            stringToRow rowString

        location =
            (loc row col)

        playerString =
            String.slice 0 1 string

        validPlayer =
            isValidPlayerString playerString

        player =
            stringToPlayer playerString

        figureString =
            String.slice 1 2 string

        validFigure =
            isValidFigureString figureString

        figure =
            { figure = stringToFigure <| figureString
            , color = player
            }
    in
        if validRow && validCol && validPlayer && validFigure then
            Just
                { loc = location
                , color = getFieldColor location
                , figure = Just figure
                }
        else
            Nothing


isValidColString : String -> Bool
isValidColString string =
    if List.member string [ "a", "b", "c", "d", "e", "f", "g", "h" ] then
        True
    else
        False


isValidRowString : String -> Bool
isValidRowString string =
    if List.member string [ "1", "2", "3", "4", "5", "6", "7", "8" ] then
        True
    else
        False


isValidPlayerString : String -> Bool
isValidPlayerString string =
    if List.member string [ "b", "w" ] then
        True
    else
        False


isValidFigureString : String -> Bool
isValidFigureString string =
    if List.member string [ "p", "n", "r", "b", "q", "k" ] then
        True
    else
        False


stringToFigure : String -> ChessFigure
stringToFigure string =
    case string of
        "p" ->
            Pawn

        "n" ->
            Knight

        "b" ->
            Bishop

        "r" ->
            Rook

        "q" ->
            Queen

        "k" ->
            King

        _ ->
            Pawn


stringToCol : String -> Int
stringToCol string =
    case string of
        "a" ->
            0

        "b" ->
            1

        "c" ->
            2

        "d" ->
            3

        "e" ->
            4

        "f" ->
            5

        "g" ->
            6

        "h" ->
            7

        _ ->
            -1


stringToRow : String -> Int
stringToRow string =
    case string of
        "8" ->
            0

        "7" ->
            1

        "6" ->
            2

        "5" ->
            3

        "4" ->
            4

        "3" ->
            5

        "2" ->
            6

        "1" ->
            7

        _ ->
            -1


stringToPlayer : String -> Player
stringToPlayer string =
    case string of
        "w" ->
            White

        "b" ->
            Black

        _ ->
            White
