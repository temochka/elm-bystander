module Maze exposing (Grid, Vertex, carve, edges, new, vertices)

import Debug
import Dict exposing (Dict)
import Random


type alias Grid =
    { adjacencyList : AdjacencyList
    , width : Int
    , height : Int
    }


type alias Vertex =
    ( Int, Int )


type alias VertexId =
    Int


type Direction
    = North
    | East
    | South
    | West


type alias AdjacencyRecord =
    { north : Maybe VertexId
    , east : Maybe VertexId
    , south : Maybe VertexId
    , west : Maybe VertexId
    }


type alias VisitedVertices =
    Dict VertexId Bool


type alias AdjacencyList =
    Dict VertexId AdjacencyRecord


new : Int -> Int -> Grid
new width height =
    let
        cols =
            List.range 0 (width - 1)

        rows =
            List.range 0 (height - 1)

        adjacencyRecord i j =
            let
                north =
                    toVertexId width ( i - 1, j )

                east =
                    toVertexId width ( i, j + 1 )

                south =
                    toVertexId width ( i + 1, j )

                west =
                    toVertexId width ( i, j - 1 )
            in
            { north =
                if i > 0 then
                    Just north

                else
                    Nothing
            , east =
                if j < width - 1 then
                    Just east

                else
                    Nothing
            , south =
                if i < height - 1 then
                    Just south

                else
                    Nothing
            , west =
                if j > 0 then
                    Just west

                else
                    Nothing
            }
    in
    { adjacencyList =
        rows
            |> List.concatMap (\i -> List.map (\j -> ( i * width + j, adjacencyRecord i j )) cols)
            |> Dict.fromList
    , width = width
    , height = height
    }


toVertexId : Int -> Vertex -> VertexId
toVertexId width ( i, j ) =
    i * width + j


vertexById : Int -> VertexId -> Vertex
vertexById width vertexId =
    ( vertexId // width, modBy width vertexId )


vertices : Grid -> List Vertex
vertices { adjacencyList, width } =
    adjacencyList |> Dict.keys |> List.map (vertexById width)


edges : Grid -> List ( Vertex, Vertex )
edges { width, height, adjacencyList } =
    let
        vertexEdges vertexId =
            [ Dict.get vertexId adjacencyList |> Maybe.andThen .north
            , Dict.get vertexId adjacencyList |> Maybe.andThen .east
            , Dict.get vertexId adjacencyList |> Maybe.andThen .south
            , Dict.get vertexId adjacencyList |> Maybe.andThen .west
            ]
                |> List.filterMap identity
                |> List.map (Tuple.pair vertexId)

        reducer ( vertexA, vertexB ) dict =
            if Dict.member ( vertexB, vertexA ) dict then
                dict

            else
                Dict.insert ( vertexA, vertexB ) True dict
    in
    adjacencyList
        |> Dict.keys
        |> List.concatMap vertexEdges
        |> List.foldl reducer Dict.empty
        |> Dict.keys
        |> List.map (Tuple.mapBoth (vertexById width) (vertexById width))


randomDirection =
    Random.uniform North [ East, South, West ]


randomStartPoint : AdjacencyList -> Random.Generator VertexId
randomStartPoint adjacencyList =
    adjacencyList
        |> Dict.keys
        |> Random.uniform -1


randomEndPoint : AdjacencyList -> VertexId -> Random.Generator VertexId
randomEndPoint adjacencyList startVertexId =
    let
        borders =
            adjacencyList
                |> Dict.keys
                |> List.filter (\v -> Dict.get v adjacencyList |> Maybe.map isBorder |> Maybe.withDefault False)
    in
    borders
        |> List.filter ((/=) startVertexId)
        |> Random.uniform -1


isBorder : AdjacencyRecord -> Bool
isBorder { north, east, south, west } =
    north == Nothing || east == Nothing || south == Nothing || west == Nothing


carve : Grid -> Random.Generator ( Vertex, Vertex )
carve { adjacencyList, width } =
    let
        visitedVertices : VisitedVertices
        visitedVertices =
            Dict.empty
    in
    randomStartPoint adjacencyList
        |> Random.andThen (\startVertexId -> Random.map (Tuple.pair startVertexId) (randomEndPoint adjacencyList startVertexId))
        |> Random.map (Tuple.mapBoth (vertexById width) (vertexById width))
