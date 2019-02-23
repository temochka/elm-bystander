module Renderer exposing (board, getDimensions, gridColor, objects, pathColor, pathOnGrid, render, renderEdges, renderPath, renderStartNode)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import MazePanel
import Model exposing (..)
import QuadGraph
import Svg
import Svg.Attributes


type alias Dimensions =
    { cellWidth : Float
    , paddingTop : Float
    , paddingLeft : Float
    , strokeWidth : Float
    , width : Int
    , height : Int
    }


getDimensions : MazePanel.Grid -> Dimensions
getDimensions { width, height } =
    let
        maxCellWidth =
            canvasSide / toFloat (max width height - 1)

        paddingModifier =
            min (0.4 + logBase 2 (toFloat (max width height)) / 10) 0.95

        cellWidth =
            paddingModifier * maxCellWidth

        paddingTop =
            (canvasSide - cellWidth * toFloat (height - 1)) / 2

        paddingLeft =
            (canvasSide - cellWidth * toFloat (width - 1)) / 2

        strokeWidth =
            cellWidth / 5
    in
    { width = width
    , height = height
    , cellWidth = cellWidth
    , paddingTop = paddingTop
    , paddingLeft = paddingLeft
    , strokeWidth = strokeWidth
    }


backgroundColor =
    "#363E3F"


boardColor =
    "#F1B840"


gridColor =
    "#453612"


pathColor =
    "#FCE66D"


completedPathColor =
    "#FFFFFF"


canvasSide =
    100


finishingDirection : QuadGraph.Direction -> ( Int, Int )
finishingDirection direction =
    case direction of
        QuadGraph.North ->
            ( 0, -1 )

        QuadGraph.East ->
            ( 1, 0 )

        QuadGraph.South ->
            ( 0, 1 )

        QuadGraph.West ->
            ( -1, 0 )


renderStartNode : Dimensions -> QuadGraph.NodeId -> List (Html msg)
renderStartNode { width, height, cellWidth, paddingTop, paddingLeft, strokeWidth } startVertexId =
    let
        ( row, col ) =
            MazePanel.nodePositionOnGrid width startVertexId
    in
    [ Svg.circle
        [ Svg.Attributes.cx (String.fromFloat (paddingLeft + cellWidth * toFloat col))
        , Svg.Attributes.cy (String.fromFloat (paddingTop + cellWidth * toFloat row))
        , Svg.Attributes.r (String.fromFloat (cellWidth / 4))
        , Svg.Attributes.fill gridColor
        ]
        []
    ]


renderTail : Dimensions -> QuadGraph.NodeId -> QuadGraph.Direction -> String -> List (Html Msg)
renderTail { width, paddingLeft, paddingTop, cellWidth, strokeWidth } endVertexId finishingMove color =
    endVertexId
        |> MazePanel.nodePositionOnGrid width
        |> appendix paddingLeft paddingTop cellWidth strokeWidth color (finishingDirection finishingMove)
        |> List.singleton


renderEdges : Dimensions -> String -> List MazePanel.MazeGraphEdge -> List (Html msg)
renderEdges { width, height, cellWidth, paddingLeft, paddingTop, strokeWidth } color edges =
    List.concatMap
        (\(QuadGraph.Edge vertexIdA vertexIdB intact) ->
            let
                ( rowA, colA ) =
                    MazePanel.nodePositionOnGrid width vertexIdA

                ( rowB, colB ) =
                    MazePanel.nodePositionOnGrid width vertexIdB

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
                [ Svg.Attributes.x1 (String.fromFloat (paddingLeft + x1))
                , Svg.Attributes.y1 (String.fromFloat (paddingTop + y1))
                , Svg.Attributes.x2 (String.fromFloat (paddingLeft + x2))
                , Svg.Attributes.y2 (String.fromFloat (paddingTop + y2))
                , Svg.Attributes.stroke color
                , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                , Svg.Attributes.strokeLinecap "round"
                ]
                []
            ]
                ++ (if not intact then
                        [ Svg.line
                            [ Svg.Attributes.x1 (String.fromFloat (paddingLeft + x1 + dx * (2 / 5)))
                            , Svg.Attributes.y1 (String.fromFloat (paddingTop + y1 + dy * (2 / 5)))
                            , Svg.Attributes.x2 (String.fromFloat (paddingLeft + x2 - dx * (2 / 5)))
                            , Svg.Attributes.y2 (String.fromFloat (paddingTop + y2 - dy * (2 / 5)))
                            , Svg.Attributes.stroke boardColor
                            , Svg.Attributes.strokeWidth (String.fromFloat (1.2 * strokeWidth))
                            ]
                            []
                        ]

                    else
                        []
                   )
        )
        edges


renderPathHead : Dimensions -> List MazePanel.NodePositionOnGrid -> List (Html msg)
renderPathHead { cellWidth, paddingLeft, paddingTop, strokeWidth } path =
    path
        |> List.reverse
        |> List.head
        |> Maybe.map
            (\( row, col ) ->
                [ Svg.circle
                    [ Svg.Attributes.cx (String.fromFloat (paddingLeft + cellWidth * toFloat col))
                    , Svg.Attributes.cy (String.fromFloat (paddingTop + cellWidth * toFloat row))
                    , Svg.Attributes.r (String.fromFloat (cellWidth / 3))
                    ]
                    []
                ]
            )
        |> Maybe.withDefault []


renderPath : Dimensions -> String -> Maybe String -> List MazePanel.NodePositionOnGrid -> List (Html msg)
renderPath ({ cellWidth, paddingLeft, paddingTop, strokeWidth } as dimensions) color blink path =
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
        (renderPathHead dimensions path
            ++ List.map
                (\( ( rowA, colA ), ( rowB, colB ) ) ->
                    Svg.line
                        [ Svg.Attributes.x1 (String.fromFloat (paddingLeft + (cellWidth * toFloat colA)))
                        , Svg.Attributes.y1 (String.fromFloat (paddingTop + (cellWidth * toFloat rowA)))
                        , Svg.Attributes.x2 (String.fromFloat (paddingLeft + (cellWidth * toFloat colB)))
                        , Svg.Attributes.y2 (String.fromFloat (paddingTop + (cellWidth * toFloat rowB)))
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


pathOnGrid : Dimensions -> List QuadGraph.NodeId -> List MazePanel.NodePositionOnGrid
pathOnGrid dimensions =
    List.map (MazePanel.nodePositionOnGrid dimensions.width)


objects : Dimensions -> Model -> List (Html Msg)
objects dimensions { gameState, animations } =
    case gameState of
        Loading ->
            []

        Playing _ game ->
            renderStartNode dimensions game.start
                ++ renderEdges dimensions gridColor (QuadGraph.edges game.grid.graph)
                ++ renderTail dimensions game.end game.finishingMove gridColor
                ++ renderPath dimensions pathColor (List.head animations |> Maybe.map (\(BlinkPath color) -> color)) (pathOnGrid dimensions game.playerPath)

        Completed _ game ->
            renderStartNode dimensions game.start
                ++ renderEdges dimensions gridColor (QuadGraph.edges game.grid.graph)
                ++ renderTail dimensions game.end game.finishingMove pathColor
                ++ renderPath dimensions completedPathColor Nothing (pathOnGrid dimensions game.playerPath)
                ++ renderTail dimensions game.end game.finishingMove completedPathColor


board : List (Html msg) -> Html msg
board entities =
    Svg.svg
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
            :: entities
        )


appendix : Float -> Float -> Float -> Float -> String -> ( Int, Int ) -> MazePanel.NodePositionOnGrid -> Html msg
appendix paddingLeft paddingTop cellWidth strokeWidth strokeColor ( tailVectorX, tailVectorY ) ( row, col ) =
    Svg.line
        [ Svg.Attributes.x1 (String.fromFloat (paddingLeft + (cellWidth * toFloat col)))
        , Svg.Attributes.y1 (String.fromFloat (paddingTop + (cellWidth * toFloat row)))
        , Svg.Attributes.x2 (String.fromFloat (paddingLeft + (cellWidth * toFloat col + (0.3 * cellWidth * toFloat tailVectorX))))
        , Svg.Attributes.y2 (String.fromFloat (paddingTop + (cellWidth * toFloat row + (0.3 * cellWidth * toFloat tailVectorY))))
        , Svg.Attributes.stroke strokeColor
        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
        , Svg.Attributes.strokeLinecap "round"
        ]
        []


nextLevelButton : String -> Bool -> Html Msg
nextLevelButton label enabled =
    Html.button
        [ onClick
            (if enabled then
                NextLevel

             else
                Nop
            )
        , style "background-color" "#51817e"
        , style "border" "none"
        , style "border-radius" "10px"
        , style "font-family" "Arial, Helvetica, sans-serif"
        , style "font-size" "24px"
        , style "color" "#FCF2E8"
        , style "display" "inline-block"
        , style "box-shadow" "0px 5px 0px 0px rgba(13,35,32,1)"
        , style "padding" "0.4em 0.6em"
        , style "margin-top" "0.5em"
        ]
        [ Html.text label ]


newGameButton : String -> Html Msg
newGameButton label =
    Html.button
        [ onClick NewGame
        , style "background-color" "#51817e"
        , style "border" "none"
        , style "border-radius" "10px"
        , style "font-family" "Arial, Helvetica, sans-serif"
        , style "font-size" "24px"
        , style "color" "#FCF2E8"
        , style "display" "inline-block"
        , style "box-shadow" "0px 5px 0px 0px rgba(13,35,32,1)"
        , style "padding" "0.4em 0.6em"
        , style "margin-top" "0.5em"
        ]
        [ Html.text label ]


render : Model -> Html Msg
render model =
    let
        buttons =
            case model.gameState of
                Playing Player _ ->
                    []

                Completed Player _ ->
                    [ nextLevelButton "Next Level ⏎" True ]

                _ ->
                    [ newGameButton "Play ⏎" ]

        gameObjects =
            case model.gameState of
                Playing _ game ->
                    objects (getDimensions game.grid) model

                Completed _ game ->
                    objects (getDimensions game.grid) model

                _ ->
                    []
    in
    Html.div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "background" backgroundColor
        , style "text-align" "center"
        ]
        [ Html.div
            [ style "margin" "5vmin auto"
            , style "width" "80vmin"
            ]
            (board gameObjects :: buttons)
        ]
