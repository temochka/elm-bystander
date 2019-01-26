module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events as Events
import Debug
import Dict
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Maze
import Random
import Svg
import Svg.Attributes


type Direction
    = Left
    | Right
    | Up
    | Down
    | None


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map (KeyPress << toDirection) (Decode.field "key" Decode.string)


finishingDirection : Maze.Direction -> ( Int, Int )
finishingDirection direction =
    case direction of
        Maze.North ->
            ( 0, -1 )

        Maze.East ->
            ( 1, 0 )

        Maze.South ->
            ( 0, 1 )

        Maze.West ->
            ( -1, 0 )


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowDown" ->
            Down

        "ArrowUp" ->
            Up

        _ ->
            None


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type GameState
    = Loading
    | Playing Maze.Game
    | Completed Maze.Game


type alias Model =
    { maze : Maze.Grid
    , gameState : GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        maze =
            Maze.new 5 5
    in
    ( { maze = maze, gameState = Loading }, Random.generate SetGame (Maze.makeGame maze) )



-- UPDATE


type Msg
    = UpdateMaze Maze.Grid
    | SetGame ( Maze.Grid, Maze.Game )
    | KeyPress Direction


directionToAccessor : Direction -> (Maze.AdjacencyRecord -> Maybe Maze.VertexId)
directionToAccessor direction =
    case direction of
        Left ->
            \record -> record.west |> Maybe.andThen Maze.passConnection

        Right ->
            \record -> record.east |> Maybe.andThen Maze.passConnection

        Up ->
            \record -> record.north |> Maybe.andThen Maze.passConnection

        Down ->
            \record -> record.south |> Maybe.andThen Maze.passConnection

        None ->
            always Nothing


handleKeyPress : Direction -> Maze.Grid -> Maze.Game -> ( Model, Cmd Msg )
handleKeyPress direction grid game =
    case game.path of
        currentVertex :: previousVertex :: _ ->
            let
                potentialNextVertex =
                    Maze.go grid currentVertex (directionToAccessor direction)
            in
            case potentialNextVertex of
                Just nextVertex ->
                    if nextVertex == previousVertex then
                        ( { maze = grid, gameState = Playing { game | path = List.tail game.path |> Maybe.withDefault [] } }, Cmd.none )

                    else
                        ( { maze = grid, gameState = Playing { game | path = nextVertex :: game.path } }, Cmd.none )

                Nothing ->
                    ( { maze = grid, gameState = Playing game }, Cmd.none )

        currentVertex :: _ ->
            let
                potentialNextVertex =
                    Maze.go grid currentVertex (directionToAccessor direction)
            in
            case potentialNextVertex of
                Just nextVertex ->
                    ( { maze = grid, gameState = Playing { game | path = nextVertex :: game.path } }, Cmd.none )

                Nothing ->
                    ( { maze = grid, gameState = Playing game }, Cmd.none )

        _ ->
            ( { maze = grid, gameState = Playing game }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMaze maze ->
            ( { model | maze = maze }, Cmd.none )

        SetGame ( grid, game ) ->
            ( { model | maze = grid, gameState = Playing game }, Cmd.none )

        KeyPress direction ->
            case model.gameState of
                Playing game ->
                    handleKeyPress direction model.maze game

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onKeyDown keyDecoder



-- VIEW


boardColor =
    "#F1B840"


gridColor =
    "#453612"


pathColor =
    "#FCE66D"


view : Model -> Html Msg
view { maze, gameState } =
    let
        cellWidth =
            85 / toFloat maze.width

        strokeWidth =
            cellWidth / 5

        renderStartNode ( row, col ) =
            [ Svg.circle
                [ Svg.Attributes.cx (String.fromFloat (cellWidth + cellWidth * toFloat col))
                , Svg.Attributes.cy (String.fromFloat (cellWidth + cellWidth * toFloat row))
                , Svg.Attributes.r (String.fromFloat (cellWidth / 4))
                , Svg.Attributes.fill gridColor
                ]
                []
            ]

        renderTail endPoint finishingMove color =
            endPoint
                |> appendix cellWidth strokeWidth color (finishingDirection finishingMove)
                |> List.singleton

        renderEdges =
            List.concatMap
                (\(Maze.Edge ( rowA, colA ) ( rowB, colB ) intact) ->
                    let
                        x1 =
                            cellWidth * toFloat colA

                        y1 =
                            cellWidth * toFloat rowA

                        x2 =
                            cellWidth * toFloat colB

                        y2 =
                            cellWidth * toFloat rowB

                        dx =
                            x2 - x1

                        dy =
                            y2 - y1
                    in
                    [ Svg.line
                        [ Svg.Attributes.x1 (String.fromFloat (cellWidth + x1))
                        , Svg.Attributes.y1 (String.fromFloat (cellWidth + y1))
                        , Svg.Attributes.x2 (String.fromFloat (cellWidth + x2))
                        , Svg.Attributes.y2 (String.fromFloat (cellWidth + y2))
                        , Svg.Attributes.stroke gridColor
                        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
                    ]
                        ++ (if not intact then
                                [ Svg.line
                                    [ Svg.Attributes.x1 (String.fromFloat (cellWidth + x1 + dx * (2 / 5)))
                                    , Svg.Attributes.y1 (String.fromFloat (cellWidth + y1 + dy * (2 / 5)))
                                    , Svg.Attributes.x2 (String.fromFloat (cellWidth + x2 - dx * (2 / 5)))
                                    , Svg.Attributes.y2 (String.fromFloat (cellWidth + y2 - dy * (2 / 5)))
                                    , Svg.Attributes.stroke boardColor
                                    , Svg.Attributes.strokeWidth (String.fromFloat (1.1 * strokeWidth))
                                    ]
                                    []
                                ]

                            else
                                []
                           )
                )
                (Maze.edges maze)

        renderPath path =
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
                (List.map2 Tuple.pair path (Maybe.withDefault [] (List.tail path)))

        renderPathHead path =
            List.reverse path
                |> List.head
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

        objects =
            case gameState of
                Loading ->
                    []

                Playing game ->
                    renderStartNode game.start
                        ++ renderEdges
                        ++ renderTail game.end game.finishingMove gridColor
                        ++ renderPathHead game.path
                        ++ renderPath game.path

                Completed game ->
                    renderStartNode game.start
                        ++ renderEdges
                        ++ renderTail game.end game.finishingMove pathColor
                        ++ renderPathHead game.path
                        ++ renderPath game.path
                        ++ renderTail game.end game.finishingMove pathColor
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
                    , Svg.Attributes.fill boardColor
                    ]
                    []
                    :: objects
                )
            ]
        ]


appendix : Float -> Float -> String -> ( Int, Int ) -> Maze.Vertex -> Html Msg
appendix cellWidth strokeWidth strokeColor ( tailVectorX, tailVectorY ) ( row, col ) =
    Svg.line
        [ Svg.Attributes.x1 (String.fromFloat (cellWidth + (cellWidth * toFloat col)))
        , Svg.Attributes.y1 (String.fromFloat (cellWidth + (cellWidth * toFloat row)))
        , Svg.Attributes.x2 (String.fromFloat (cellWidth + (cellWidth * toFloat col + toFloat (5 * tailVectorX))))
        , Svg.Attributes.y2 (String.fromFloat (cellWidth + (cellWidth * toFloat row + toFloat (5 * tailVectorY))))
        , Svg.Attributes.stroke strokeColor
        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
        , Svg.Attributes.strokeLinecap "round"
        ]
        []
