module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Maze
import Random
import Svg
import Svg.Attributes


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { maze : Maze.Grid
    , startPoint : Maybe Maze.Vertex
    , endPoint : Maybe Maze.Vertex
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        maze =
            Maze.new 5 5
    in
    ( { maze = maze, startPoint = Nothing, endPoint = Nothing }, Random.generate SetStartAndEnd (Maze.carve maze) )



-- UPDATE


type Msg
    = UpdateMaze Maze.Grid
    | SetStartAndEnd ( Maze.Vertex, Maze.Vertex )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMaze maze ->
            ( { model | maze = maze }, Cmd.none )

        SetStartAndEnd ( start, end ) ->
            Debug.log "Updated Model: " ( { model | startPoint = Just start, endPoint = Just end }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { maze, startPoint, endPoint } =
    let
        nodes =
            List.map
                (\(( row, col ) as point) ->
                    Svg.circle
                        [ Svg.Attributes.cx (String.fromInt (40 + 40 * col))
                        , Svg.Attributes.cy (String.fromInt (40 + 40 * row))
                        , Svg.Attributes.r "10"
                        , Svg.Attributes.fill
                            (if Maybe.map ((==) point) startPoint |> Maybe.withDefault False then
                                "green"

                             else if Maybe.map ((==) point) endPoint |> Maybe.withDefault False then
                                "red"

                             else
                                "black"
                            )
                        ]
                        []
                )
                (Maze.vertices maze)

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
                (Maze.edges maze)
    in
    Svg.svg
        [ Svg.Attributes.width "800"
        , Svg.Attributes.height "800"
        ]
        (nodes ++ edges)
