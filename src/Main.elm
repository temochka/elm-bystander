module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Dict
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
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
    , game : Maybe Maze.Game
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        maze =
            Maze.new 5 5
    in
    ( { maze = maze, game = Nothing }, Random.generate SetGame (Maze.makeGame maze) )



-- UPDATE


type Msg
    = UpdateMaze Maze.Grid
    | SetGame (Maybe Maze.Game)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMaze maze ->
            ( { model | maze = maze }, Cmd.none )

        SetGame game ->
            ( { model | game = game }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


gridColor =
    "#453612"


pathColor =
    "#FCE66D"


view : Model -> Html Msg
view { maze, game } =
    let
        startPoint =
            Maybe.map .start game

        endPoint =
            Maybe.map .end game

        cellWidth =
            85 / toFloat maze.width

        strokeWidth =
            cellWidth / 5

        tailVector =
            endPoint
                |> Maybe.map (Maze.toVertexId maze.width)
                |> Maybe.andThen (\vertexId -> Dict.get vertexId maze.adjacencyList)
                |> Maybe.andThen Maze.unavailableDirection
                |> Maybe.withDefault ( 0, 0 )

        startNode =
            startPoint
                |> Maybe.map
                    (\( row, col ) ->
                        [ Svg.circle
                            [ Svg.Attributes.cx (String.fromFloat (cellWidth + cellWidth * toFloat col))
                            , Svg.Attributes.cy (String.fromFloat (cellWidth + cellWidth * toFloat row))
                            , Svg.Attributes.r (String.fromFloat (cellWidth / 4))
                            , Svg.Attributes.fill gridColor
                            ]
                            []
                        ]
                    )
                |> Maybe.withDefault []

        tail =
            endPoint
                |> Maybe.map (appendix cellWidth strokeWidth tailVector)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        edges =
            List.map
                (\( ( rowA, colA ), ( rowB, colB ) ) ->
                    Svg.line
                        [ Svg.Attributes.x1 (String.fromFloat (cellWidth + (cellWidth * toFloat colA)))
                        , Svg.Attributes.y1 (String.fromFloat (cellWidth + (cellWidth * toFloat rowA)))
                        , Svg.Attributes.x2 (String.fromFloat (cellWidth + (cellWidth * toFloat colB)))
                        , Svg.Attributes.y2 (String.fromFloat (cellWidth + (cellWidth * toFloat rowB)))
                        , Svg.Attributes.stroke gridColor
                        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
                )
                (Maze.edges maze)

        path =
            List.map
                (\( ( rowA, colA ), ( rowB, colB ) ) ->
                    Svg.line
                        [ Svg.Attributes.x1 (String.fromFloat (cellWidth + (cellWidth * toFloat colA)))
                        , Svg.Attributes.y1 (String.fromFloat (cellWidth + (cellWidth * toFloat rowA)))
                        , Svg.Attributes.x2 (String.fromFloat (cellWidth + (cellWidth * toFloat colB)))
                        , Svg.Attributes.y2 (String.fromFloat (cellWidth + (cellWidth * toFloat rowB)))
                        , Svg.Attributes.stroke pathColor
                        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
                )
                (Maybe.map .path game |> Maybe.withDefault [] |> (\list -> List.map2 Tuple.pair list (Maybe.withDefault [] (List.tail list))))

        pathHead =
            game
                |> Maybe.map .path
                |> Maybe.andThen (List.head << List.reverse)
                |> Maybe.map
                    (\( row, col ) ->
                        [ Svg.circle
                            [ Svg.Attributes.cx (String.fromFloat (cellWidth + cellWidth * toFloat col))
                            , Svg.Attributes.cy (String.fromFloat (cellWidth + cellWidth * toFloat row))
                            , Svg.Attributes.r (String.fromFloat (cellWidth / 4))
                            , Svg.Attributes.fill pathColor
                            ]
                            []
                        ]
                    )
                |> Maybe.withDefault []

        pathTail =
            game
                |> Maybe.map .path
                |> Maybe.andThen List.head
                |> Maybe.map (appendix cellWidth strokeWidth tailVector)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    Html.div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "background" "#363E3F"
        ]
        [ Html.div
            [ style "margin" "10vmin auto"
            , style "width" "80vmin"
            ]
            [ Svg.svg
                [ Svg.Attributes.viewBox "0 0 100 100"
                ]
                (Svg.rect
                    [ Svg.Attributes.x "0"
                    , Svg.Attributes.y "0"
                    , Svg.Attributes.rx "1"
                    , Svg.Attributes.ry "1"
                    , Svg.Attributes.width "100"
                    , Svg.Attributes.height "100"
                    , Svg.Attributes.fill "#F1B840"
                    ]
                    []
                    :: (startNode ++ edges ++ pathHead ++ path ++ pathTail)
                )
            ]
        ]


appendix : Float -> Float -> ( Int, Int ) -> Maze.Vertex -> Html Msg
appendix cellWidth strokeWidth ( tailVectorX, tailVectorY ) ( row, col ) =
    Svg.line
        [ Svg.Attributes.x1 (String.fromFloat (cellWidth + (cellWidth * toFloat col)))
        , Svg.Attributes.y1 (String.fromFloat (cellWidth + (cellWidth * toFloat row)))
        , Svg.Attributes.x2 (String.fromFloat (cellWidth + (cellWidth * toFloat col + toFloat (5 * tailVectorX))))
        , Svg.Attributes.y2 (String.fromFloat (cellWidth + (cellWidth * toFloat row + toFloat (5 * tailVectorY))))
        , Svg.Attributes.stroke pathColor
        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
        , Svg.Attributes.strokeLinecap "round"
        ]
        []
