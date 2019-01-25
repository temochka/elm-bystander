module Maze exposing (Game, Grid, Vertex, edges, makeGame, new, toVertexId, unavailableDirection, vertices)

import Debug
import Dict exposing (Dict)
import Random
import Random.List


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


type alias Game =
    { start : Vertex
    , end : Vertex
    , path : List Vertex
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



-- randomDirection : List Direction -> Random.Generator Direction
-- randomDirection directions =
--     Random.List.shuffle directions


randomListElement : a -> List a -> Random.Generator a
randomListElement default list =
    case list of
        [] ->
            Random.constant default

        x :: xs ->
            Random.uniform x xs


randomStartPoint : AdjacencyList -> Random.Generator VertexId
randomStartPoint adjacencyList =
    adjacencyList
        |> Dict.keys
        |> randomListElement -1


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
        |> randomListElement -1


isBorder : AdjacencyRecord -> Bool
isBorder { north, east, south, west } =
    north == Nothing || east == Nothing || south == Nothing || west == Nothing


juxt : (a -> Random.Generator b) -> a -> Random.Generator ( a, b )
juxt f x =
    Random.map (\y -> ( x, y )) (f x)


availableDirections : AdjacencyRecord -> List VertexId
availableDirections record =
    [ .north, .east, .south, .west ]
        |> List.filterMap (\method -> method record)


unavailableDirection : AdjacencyRecord -> Maybe ( Int, Int )
unavailableDirection record =
    if record.north == Nothing then
        Just ( 0, -1 )

    else if record.east == Nothing then
        Just ( 1, 0 )

    else if record.south == Nothing then
        Just ( 0, 1 )

    else if record.west == Nothing then
        Just ( -1, 0 )

    else
        Just ( 0, 0 )


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        x :: [] ->
            Just x

        x :: xs ->
            last xs


makeGame : Grid -> Random.Generator (Maybe Game)
makeGame { adjacencyList, width } =
    let
        visitedVertices : VisitedVertices
        visitedVertices =
            Dict.empty

        find : (a -> Maybe b) -> List a -> Maybe b
        find f list =
            List.filterMap f list |> List.head

        carvePath : List VertexId -> ( VertexId, VertexId ) -> Maybe Game
        carvePath path ( current, end ) =
            if current == end && List.length path >= width * 2 then
                Just { start = vertexById width (last path |> Maybe.withDefault 0), end = vertexById width current, path = List.map (vertexById width) path }

            else if current == end then
                Nothing

            else
                Dict.get current adjacencyList
                    |> Maybe.map availableDirections
                    |> Maybe.map (List.filter (\direction -> not (List.member direction path)))
                    |> Maybe.andThen (find (\vertexId -> carvePath (vertexId :: path) ( vertexId, end )))
    in
    randomStartPoint adjacencyList
        |> Random.andThen (juxt (randomEndPoint adjacencyList))
        |> Random.map (carvePath [])
