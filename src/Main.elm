module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Maze
import Svg
import Svg.Attributes


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Maze.Grid


init : Model
init =
    Maze.new3x3



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            model



-- VIEW


view : Model -> Html Msg
view grid =
    let
        nodes =
            List.map
                (\( row, col ) ->
                    Svg.circle
                        [ Svg.Attributes.cx (String.fromInt (40 + 40 * col))
                        , Svg.Attributes.cy (String.fromInt (40 + 40 * row))
                        , Svg.Attributes.r "10"
                        ]
                        []
                )
                (Maze.vertices grid)

        edges =
            List.map
                (\( ( rowA, colA ), ( rowB, colB ) ) ->
                    Svg.line
                        [ Svg.Attributes.x1 (String.fromInt (40 + (40 * colA)))
                        , Svg.Attributes.y1 (String.fromInt (40 + (40 * rowA)))
                        , Svg.Attributes.x2 (String.fromInt (40 + (40 * colB)))
                        , Svg.Attributes.y2 (String.fromInt (40 + (40 * rowB)))
                        , Svg.Attributes.stroke "black"
                        ]
                        []
                )
                (Maze.edges grid)
    in
    Svg.svg
        [ Svg.Attributes.width "800"
        , Svg.Attributes.height "800"
        ]
        (nodes ++ edges)
