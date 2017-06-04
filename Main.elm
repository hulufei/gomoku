module Main exposing (..)

import String
import Regex
import Tuple exposing (first, second)
import Array exposing (Array)
import Array2D exposing (Array2D)
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (..)


-- Model


type Piece
    = Black
    | White
    | Empty


type MatchResult
    = BlackWin
    | WhiteWin
    | Draw
    | Continue


type alias Row =
    Int


type alias Col =
    Int


type alias Board =
    Array2D Piece


type alias Model =
    { board : Board
    , record : List Board
    , playBackStep : Int
    , result : MatchResult
    }


initBoard : Board
initBoard =
    Array2D.repeat 15 15 Empty


initModel : Model
initModel =
    { board = initBoard
    , record = []
    , playBackStep = 0
    , result = Continue
    }



-- Update


type Msg
    = Place Row Col
    | Undo
    | NewGame
    | PlayBack String
    | NoOp


isPlayBackMode : Model -> Bool
isPlayBackMode model =
    model.playBackStep /= List.length model.record


isNextBlackTurn : Model -> Bool
isNextBlackTurn model =
    model.playBackStep % 2 == 0


getHorizonLines : Array2D String -> Array String
getHorizonLines stringBoard =
    let
        getRow index =
            Array2D.getRow index stringBoard
                |> Maybe.withDefault Array.empty
                |> Array.toList
                |> String.join ""
    in
        Array.initialize (Array2D.rows stringBoard) identity
            |> Array.map getRow


getVerticalLines : Array2D String -> Array String
getVerticalLines stringBoard =
    let
        getColumn index =
            Array2D.getColumn index stringBoard
                |> Array.map (Maybe.withDefault "E")
                |> Array.toList
                |> String.join ""
    in
        Array.initialize (Array2D.columns stringBoard) identity
            |> Array.map getColumn


getDiagonalLines : Array2D String -> Array String
getDiagonalLines stringBoard =
    let
        getPiece row col =
            Array2D.get row col stringBoard
                |> Maybe.withDefault "E"

        getAround row col offset =
            ( ( getPiece (row - offset) (col + offset)
              , getPiece (row + offset) (col - offset)
              )
            , ( getPiece (row - offset) (col - offset)
              , getPiece (row + offset) (col + offset)
              )
            )

        concatString piece ( t1, t2 ) =
            List.concat [ t1, [ piece ], t2 ]
                |> String.join ""

        concatLines piece ( slashLine, reverseLine ) =
            [ List.unzip slashLine |> concatString piece
            , List.unzip reverseLine |> concatString piece
            ]

        getLines row col piece =
            if piece == "x" then
                "-"
            else
                List.range 1 4
                    |> List.map (getAround row col)
                    |> List.unzip
                    |> concatLines piece
                    |> String.join "-"
    in
        Array2D.indexedMap getLines stringBoard
            |> getHorizonLines


checkMatchResult : Board -> MatchResult
checkMatchResult board =
    let
        pieceToString piece =
            case piece of
                Black ->
                    "b"

                White ->
                    "w"

                Empty ->
                    "x"

        stringBoard =
            Array2D.map pieceToString board

        lines =
            getHorizonLines stringBoard
                |> Array.append (getVerticalLines stringBoard)
                |> Array.append (getDiagonalLines stringBoard)
                |> Array.toList
                |> String.join "-"

        blackWinPattern =
            Regex.regex "b{5}"

        whiteWinPattern =
            Regex.regex "w{5}"
    in
        if Regex.contains blackWinPattern lines then
            BlackWin
        else if Regex.contains whiteWinPattern lines then
            WhiteWin
        else if not <| String.contains "x" lines then
            Draw
        else
            Continue


update : Msg -> Model -> Model
update msg model =
    case msg of
        Place row col ->
            let
                { playBackStep, record, board } =
                    model

                piece =
                    if isNextBlackTurn model then
                        Black
                    else
                        White

                newBoard =
                    Array2D.set row col piece board

                recordCount =
                    List.length record
            in
                if model.result /= Continue then
                    model
                else if isPlayBackMode model then
                    { model
                        | board = newBoard
                        , record = newBoard :: List.drop (recordCount - playBackStep) record
                        , playBackStep = 1 + playBackStep
                        , result = checkMatchResult newBoard
                    }
                else
                    { model
                        | board = newBoard
                        , record = newBoard :: record
                        , playBackStep = 1 + recordCount
                        , result = checkMatchResult newBoard
                    }

        Undo ->
            case model.record of
                [] ->
                    model

                _ :: remain ->
                    { model
                        | board = List.head remain |> Maybe.withDefault initBoard
                        , record = remain
                        , playBackStep = List.length remain
                        , result = Continue
                    }

        PlayBack step ->
            let
                playBackStep =
                    String.toInt step |> Result.withDefault 0

                recordCount =
                    List.length model.record

                board =
                    List.drop (recordCount - playBackStep) model.record
                        |> List.head
                        |> Maybe.withDefault initBoard
            in
                { model
                    | playBackStep = playBackStep
                    , board = board
                    , result =
                        if playBackStep == recordCount then
                            checkMatchResult board
                        else
                            Continue
                }

        NewGame ->
            initModel

        _ ->
            model



-- View


viewPiece : Row -> Col -> Piece -> Html Msg
viewPiece row column piece =
    let
        attrs =
            case piece of
                Black ->
                    ( "piece black", NoOp )

                White ->
                    ( "piece white", NoOp )

                Empty ->
                    ( "piece empty", Place row column )
    in
        td [ class "cell" ]
            [ span
                [ class <| first attrs
                , onClick <| second attrs
                ]
                []
            ]


viewBoard : Board -> Html Msg
viewBoard board =
    let
        getHtmlRow index =
            Array2D.indexedMap viewPiece board
                |> Array2D.getRow index
                |> Maybe.withDefault Array.empty
                |> Array.toList
                |> tr [ class "row" ]
    in
        Array.initialize (Array2D.rows board) identity
            |> Array.toList
            |> List.map getHtmlRow
            |> tbody []
            |> List.singleton
            |> table [ class "board" ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        turn =
            if isNextBlackTurn model then
                "Black"
            else
                "White"

        title =
            case model.result of
                BlackWin ->
                    span [ class "win" ] [ text "Black Win!" ]

                WhiteWin ->
                    span [ class "lose" ] [ text "White Win!" ]

                Draw ->
                    span [ class "draw" ] [ text "Draw" ]

                _ ->
                    text ("Turn: " ++ turn)

        recordCount =
            toString <| List.length model.record

        recordValue =
            toString model.playBackStep
    in
        div [ class "sidebar" ]
            [ p [ class "title" ] [ title ]
            , p [] [ text ("Move " ++ recordValue) ]
            , p []
                [ input
                    [ type_ "range"
                    , H.min "0"
                    , H.max recordCount
                    , value recordValue
                    , onInput PlayBack
                    ]
                    []
                ]
            , p []
                [ button
                    [ disabled <| isPlayBackMode model
                    , onClick Undo
                    ]
                    [ text "Undo"
                    ]
                ]
            , p []
                [ button
                    [ class "new-game"
                    , onClick NewGame
                    ]
                    [ text "New Game" ]
                ]
            , a
                [ href "https://github.com/hulufei/gomoku"
                , class "github"
                ]
                [ text "Github" ]
            ]


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "container", True )
            , ( "mode-playback", isPlayBackMode model )
            , ( "game-over", model.result /= Continue )
            ]
        ]
        [ viewBoard model.board
        , viewSidebar model
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
