module Main exposing (..)

import Array
import String
import Tuple exposing (first, second)
import Array2D exposing (Array2D)
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (..)


-- Model


type Piece
    = Black
    | White
    | Empty


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
    }


initBoard : Board
initBoard =
    Array2D.repeat 15 15 Empty


initModel : Model
initModel =
    { board = initBoard
    , record = []
    , playBackStep = 0
    }



-- Update


type MatchResult
    = Win
    | Lose
    | Draw


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        Place row col ->
            let
                piece =
                    if isNextBlackTurn model then
                        Black
                    else
                        White

                newBoard =
                    Array2D.set row col piece model.board
            in
                if isPlayBackMode model then
                    -- Playback mode freezing place
                    model
                else
                    { model
                        | board = newBoard
                        , record = newBoard :: model.record
                        , playBackStep = 1 + List.length model.record
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
                    }

        PlayBack step ->
            let
                playBackStep =
                    String.toInt step |> Result.withDefault 0

                len =
                    List.length model.record
            in
                { model
                    | playBackStep = playBackStep
                    , board =
                        List.drop (len - playBackStep) model.record
                            |> List.head
                            |> Maybe.withDefault initBoard
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
            if isPlayBackMode model then
                "-"
            else if isNextBlackTurn model then
                "Black"
            else
                "White"

        recordCount =
            toString <| List.length model.record

        recordValue =
            toString model.playBackStep
    in
        div [ class "sidebar" ]
            [ p [] [ text ("Turn: " ++ turn) ]
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
            ]


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "container", True )
            , ( "mode-playback", isPlayBackMode model )
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
