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
import Time


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


toDirection : String -> Maybe Maze.Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Just Maze.West

        "ArrowRight" ->
            Just Maze.East

        "ArrowDown" ->
            Just Maze.South

        "ArrowUp" ->
            Just Maze.North

        _ ->
            Nothing


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type GameState
    = Loading
    | Playing Maze.Game
    | Completed Maze.Game
    | Demo Maze.Game


type Animation
    = BlinkPath String


type alias Model =
    { maze : Maze.Grid
    , gameState : GameState
    , animations : List Animation
    , selectedMode : Maze.Game -> GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        maze =
            Maze.new 6 6
    in
    ( { maze = maze, gameState = Loading, animations = [], selectedMode = Demo }, Random.generate SetGame (Maze.makeGame maze) )



-- UPDATE


type Msg
    = UpdateMaze Maze.Grid
    | SetGame ( Maze.Grid, Maze.Game )
    | KeyPress (Maybe Maze.Direction)
    | AiMove Time.Posix


directionToAccessor : Maybe Maze.Direction -> (Maze.AdjacencyRecord -> Maybe Maze.VertexId)
directionToAccessor direction =
    case direction of
        Just Maze.West ->
            \record -> record.west |> Maybe.andThen Maze.passConnection

        Just Maze.East ->
            \record -> record.east |> Maybe.andThen Maze.passConnection

        Just Maze.North ->
            \record -> record.north |> Maybe.andThen Maze.passConnection

        Just Maze.South ->
            \record -> record.south |> Maybe.andThen Maze.passConnection

        Nothing ->
            always Nothing


nextAiMove : Maze.Grid -> Maze.Game -> Maybe Maze.Direction
nextAiMove grid game =
    let
        currentNode =
            List.head game.path |> Maybe.withDefault game.start

        _ =
            Debug.log "currentNode: " currentNode

        nextNode =
            game.correctPath |> List.drop (max (List.length game.path) 1) |> List.head

        _ =
            Debug.log "nextNode: " nextNode
    in
    case nextNode of
        Nothing ->
            Just game.finishingMove

        Just node ->
            Dict.get (Maze.toVertexId grid.width currentNode) grid.adjacencyList
                |> Maybe.andThen (\record -> Maze.getDirection record (Maze.toVertexId grid.width node))


handleKeyPress : (Maze.Game -> GameState) -> Maybe Maze.Direction -> Maze.Grid -> Maze.Game -> ( Model, Cmd Msg )
handleKeyPress mode direction grid game =
    case game.path of
        currentVertex :: previousVertexes ->
            let
                potentialNextVertex =
                    Maze.go grid currentVertex (directionToAccessor direction)

                previousVertex =
                    List.head previousVertexes
            in
            case potentialNextVertex of
                Just nextVertex ->
                    if potentialNextVertex == previousVertex then
                        ( { animations = [], maze = grid, selectedMode = mode, gameState = mode { game | path = List.tail game.path |> Maybe.withDefault [] } }, Cmd.none )

                    else if List.member nextVertex game.path then
                        ( { animations = [ BlinkPath "red" ], maze = grid, selectedMode = mode, gameState = mode game }, Cmd.none )

                    else
                        ( { animations = [], maze = grid, selectedMode = mode, gameState = mode { game | path = nextVertex :: game.path } }, Cmd.none )

                Nothing ->
                    if currentVertex == game.end && (direction |> Maybe.map ((==) game.finishingMove) |> Maybe.withDefault False) then
                        ( { animations = [], maze = grid, selectedMode = mode, gameState = Completed game }, Cmd.none )

                    else
                        ( { animations = [ BlinkPath "red" ], maze = grid, selectedMode = mode, gameState = mode game }, Cmd.none )

        _ ->
            ( { animations = [], maze = grid, selectedMode = mode, gameState = mode game }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg modelWithOldAnimations =
    let
        model =
            { modelWithOldAnimations | animations = [] }
    in
    case msg of
        UpdateMaze maze ->
            ( { model | maze = maze }, Cmd.none )

        SetGame ( grid, game ) ->
            ( { model | maze = grid, gameState = Demo game }, Cmd.none )

        KeyPress direction ->
            case model.gameState of
                Playing game ->
                    handleKeyPress model.selectedMode direction model.maze game

                _ ->
                    ( model, Cmd.none )

        AiMove _ ->
            case model.gameState of
                Demo game ->
                    let
                        nextMove =
                            nextAiMove model.maze game

                        _ =
                            Debug.log "nextAiMove" nextMove
                    in
                    handleKeyPress model.selectedMode nextMove model.maze game

                Completed _ ->
                    init ()

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown keyDecoder
        , Time.every 200.0 AiMove
        ]



-- VIEW


boardColor =
    "#F1B840"


gridColor =
    "#453612"


pathColor =
    "#FCE66D"


completedPathColor =
    "#FFFFFF"


view : Model -> Html Msg
view { maze, gameState, animations } =
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

        renderPathHead path =
            List.reverse path
                |> List.head
                |> Maybe.map
                    (\( row, col ) ->
                        [ Svg.circle
                            [ Svg.Attributes.cx (String.fromFloat (cellWidth + cellWidth * toFloat col))
                            , Svg.Attributes.cy (String.fromFloat (cellWidth + cellWidth * toFloat row))
                            , Svg.Attributes.r (String.fromFloat (cellWidth / 4))
                            ]
                            []
                        ]
                    )
                |> Maybe.withDefault []

        renderPath path color blink =
            [ Svg.g
                [ Svg.Attributes.id "pathAnimation"
                , Svg.Attributes.stroke color
                , Svg.Attributes.fill color
                , Svg.Attributes.class
                    (if blink == Nothing then
                        ""

                     else
                        "animatedPath"
                    )
                ]
                (renderPathHead path
                    ++ List.map
                        (\( ( rowA, colA ), ( rowB, colB ) ) ->
                            Svg.line
                                [ Svg.Attributes.x1 (String.fromFloat (cellWidth + (cellWidth * toFloat colA)))
                                , Svg.Attributes.y1 (String.fromFloat (cellWidth + (cellWidth * toFloat rowA)))
                                , Svg.Attributes.x2 (String.fromFloat (cellWidth + (cellWidth * toFloat colB)))
                                , Svg.Attributes.y2 (String.fromFloat (cellWidth + (cellWidth * toFloat rowB)))
                                , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                                , Svg.Attributes.strokeLinecap "round"
                                ]
                                []
                        )
                        (List.map2 Tuple.pair path (Maybe.withDefault [] (List.tail path)))
                    ++ [ Svg.animate [ Svg.Attributes.attributeType "XML", Svg.Attributes.attributeName "stroke", Svg.Attributes.from (blink |> Maybe.withDefault color), Svg.Attributes.to color, Svg.Attributes.dur "1s", Svg.Attributes.repeatCount "1", Svg.Attributes.begin "pathAnimation.DOMSubtreeModified" ] []
                       , Svg.animate [ Svg.Attributes.attributeType "XML", Svg.Attributes.attributeName "fill", Svg.Attributes.from (blink |> Maybe.withDefault color), Svg.Attributes.to color, Svg.Attributes.dur "1s", Svg.Attributes.repeatCount "1", Svg.Attributes.begin "pathAnimation.DOMSubtreeModified" ] []
                       ]
                )
            ]

        objects =
            case gameState of
                Loading ->
                    []

                Demo game ->
                    renderStartNode game.start
                        ++ renderEdges
                        ++ renderTail game.end game.finishingMove gridColor
                        ++ renderPath game.path pathColor (List.head animations |> Maybe.map (\(BlinkPath color) -> color))

                Playing game ->
                    renderStartNode game.start
                        ++ renderEdges
                        ++ renderTail game.end game.finishingMove gridColor
                        ++ renderPath game.path pathColor (List.head animations |> Maybe.map (\(BlinkPath color) -> color))

                Completed game ->
                    renderStartNode game.start
                        ++ renderEdges
                        ++ renderTail game.end game.finishingMove pathColor
                        ++ renderPath game.path completedPathColor Nothing
                        ++ renderTail game.end game.finishingMove completedPathColor
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
