module Renderer exposing (render)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Maze
import Model exposing (..)
import Svg
import Svg.Attributes


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


board : Model -> Html Msg
board { maze, gameState, animations } =
    let
        maxCellWidth =
            canvasSide / toFloat (max maze.width maze.height - 1)

        paddingModifier =
            min (0.4 + logBase 2 (toFloat (max maze.width maze.height)) / 10) 0.95

        cellWidth =
            paddingModifier * maxCellWidth

        paddingTop =
            (canvasSide - cellWidth * toFloat (maze.height - 1)) / 2

        paddingLeft =
            (canvasSide - cellWidth * toFloat (maze.width - 1)) / 2

        strokeWidth =
            cellWidth / 5

        renderStartNode startVertexId =
            let
                ( row, col ) =
                    Maze.vertexIdOnGrid maze.width startVertexId
            in
            [ Svg.circle
                [ Svg.Attributes.cx (String.fromFloat (paddingLeft + cellWidth * toFloat col))
                , Svg.Attributes.cy (String.fromFloat (paddingTop + cellWidth * toFloat row))
                , Svg.Attributes.r (String.fromFloat (cellWidth / 4))
                , Svg.Attributes.fill gridColor
                ]
                []
            ]

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

        renderTail endVertexId finishingMove color =
            endVertexId
                |> Maze.vertexIdOnGrid maze.width
                |> appendix paddingLeft paddingTop cellWidth strokeWidth color (finishingDirection finishingMove)
                |> List.singleton

        renderEdges =
            List.concatMap
                (\(Maze.Edge vertexIdA vertexIdB intact) ->
                    let
                        ( rowA, colA ) =
                            Maze.vertexIdOnGrid maze.width vertexIdA

                        ( rowB, colB ) =
                            Maze.vertexIdOnGrid maze.width vertexIdB

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
                        , Svg.Attributes.stroke gridColor
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
                (Maze.edges maze)

        renderPathHead path =
            List.reverse path
                |> List.head
                |> Maybe.map
                    (\( row, col ) ->
                        [ Svg.circle
                            [ Svg.Attributes.cx (String.fromFloat (paddingLeft + cellWidth * toFloat col))
                            , Svg.Attributes.cy (String.fromFloat (paddingTop + cellWidth * toFloat row))
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

        pathOnGrid path =
            path |> List.map (Maze.vertexIdOnGrid maze.width)

        objects =
            case gameState of
                Loading ->
                    []

                Playing _ game ->
                    renderStartNode game.start
                        ++ renderEdges
                        ++ renderTail game.end game.finishingMove gridColor
                        ++ renderPath (pathOnGrid game.path) pathColor (List.head animations |> Maybe.map (\(BlinkPath color) -> color))

                Completed _ game ->
                    renderStartNode game.start
                        ++ renderEdges
                        ++ renderTail game.end game.finishingMove pathColor
                        ++ renderPath (pathOnGrid game.path) completedPathColor Nothing
                        ++ renderTail game.end game.finishingMove completedPathColor
    in
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
            :: objects
        )


appendix : Float -> Float -> Float -> Float -> String -> ( Int, Int ) -> Maze.VertexOnGrid -> Html Msg
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
        , style "background-color" "#F88B33"
        , style "border" "none"
        , style "border-radius" "0.1em"
        , style "font-family" "Georgia, Times New Roman, serif"
        , style "font-size" "2em"
        , style "color" "#FCF2E8"
        , style "display" "inline-block"
        , style "padding" "0.2em 0.4em"
        , style "margin-top" "0.5em"
        ]
        [ Html.text label ]


newGameButton : String -> Html Msg
newGameButton label =
    Html.button
        [ onClick NewGame
        , style "background-color" "#F88B33"
        , style "border" "none"
        , style "border-radius" "0.1em"
        , style "font-family" "Georgia, Times New Roman, serif"
        , style "font-size" "2em"
        , style "color" "#FCF2E8"
        , style "display" "inline-block"
        , style "padding" "0.2em 0.4em"
        , style "margin-top" "0.5em"
        ]
        [ Html.text label ]


render : Model -> Html Msg
render model =
    let
        buttons =
            case model.gameState of
                Playing Player _ ->
                    [ newGameButton "New game ⓡ" ]

                Completed Player _ ->
                    [ nextLevelButton "Next Level ⏎" True ]

                _ ->
                    [ newGameButton "New game ⏎" ]
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
            (board model :: buttons)
        ]
