module MazePanel exposing (Grid, MazeGraph, MazeEdge, MazeGraphNode, MazePanel, NodePositionOnGrid, carveLoops, carvePaths, connectedNodes, new, newGrid, nodePositionOnGrid, randomStartPoint)

import Dict exposing (Dict)
import QuadGraph exposing (Direction(..), NodeId, QuadGraph)
import QuadGraph.Traverse
import QuadGraph.Traverse.Random
import Random
import Random.Extra
import Random.List
import Set exposing (Set)
import State exposing (State)


type alias MazeGraph =
    QuadGraph () Bool


type alias MazeEdge =
    QuadGraph.Edge Bool


type alias MazeGraphNode =
    QuadGraph.Node () Bool


type alias NodePositionOnGrid =
    ( Int, Int )


type alias Grid =
    { graph : MazeGraph
    , width : Int
    , height : Int
    }


type alias MazePanel =
    { start : NodeId
    , end : NodeId
    , optimalSolution : List NodeId
    , playerPath : List NodeId
    , finishingMove : Direction
    , grid : Grid
    }


newGrid : Int -> Int -> Grid
newGrid width height =
    let
        cols =
            List.range 0 (width - 1)

        rows =
            List.range 0 (height - 1)

        connections i j =
            let
                nodeId =
                    toNodeId width ( i, j )

                northNodeId =
                    toNodeId width ( i - 1, j )

                eastNodeId =
                    toNodeId width ( i, j + 1 )

                southNodeId =
                    toNodeId width ( i + 1, j )

                westNodeId =
                    toNodeId width ( i, j - 1 )
            in
            [ if i > 0 then
                Just (QuadGraph.insertEdge North nodeId northNodeId False)

              else
                Nothing
            , if j < width - 1 then
                Just (QuadGraph.insertEdge East nodeId eastNodeId False)

              else
                Nothing
            , if i < height - 1 then
                Just (QuadGraph.insertEdge South nodeId southNodeId False)

              else
                Nothing
            , if j > 0 then
                Just (QuadGraph.insertEdge West nodeId westNodeId False)

              else
                Nothing
            ]
                |> List.filterMap identity

        disconnectedGraph =
            List.range 0 (width * height - 1)
                |> List.foldl (\id graph -> QuadGraph.insertNode id () graph) QuadGraph.empty
    in
    { graph =
        rows
            |> List.concatMap (\i -> List.concatMap (connections i) cols)
            |> List.foldl (<|) disconnectedGraph
    , width = width
    , height = height
    }


toNodeId : Int -> NodePositionOnGrid -> NodeId
toNodeId width ( i, j ) =
    i * width + j


nodePositionOnGrid : Int -> NodeId -> NodePositionOnGrid
nodePositionOnGrid width nodeId =
    ( nodeId // width, modBy width nodeId )


randomListElement : a -> List a -> Random.Generator a
randomListElement default list =
    case list of
        [] ->
            Random.constant default

        x :: xs ->
            Random.uniform x xs


randomStartPoint : MazeGraph -> Random.Generator NodeId
randomStartPoint graph =
    graph
        |> Dict.keys
        |> randomListElement -1


isDisconnected : MazeGraphNode -> Bool
isDisconnected node =
    node |> QuadGraph.nodeEdges |> List.any (\e -> Nothing /= QuadGraph.resolveEdgeIf identity e) |> not


carvePaths : MazeGraph -> NodeId -> Random.Generator MazeGraph
carvePaths inputGraph startNodeId =
    let
        carve path nodeId graph =
            let
                fromNodeId =
                    path |> List.head |> Maybe.withDefault -1
            in
            QuadGraph.updateEdge fromNodeId nodeId True graph
    in
    QuadGraph.Traverse.Random.runDepthFirst carve (QuadGraph.Foldable not startNodeId inputGraph)


findDeadEnds : MazeGraph -> NodeId -> List NodeId
findDeadEnds graph startNodeId =
    QuadGraph.Traverse.foldDepthFirst
        (\path nodeId deadEnds ->
            let
                connectionsCount =
                    nodeId
                        |> QuadGraph.get graph
                        |> Maybe.map connectedNodes
                        |> Maybe.withDefault []
                        |> List.length
            in
            if connectionsCount <= 1 then
                nodeId :: deadEnds

            else
                deadEnds
        )
        []
        (QuadGraph.Foldable identity startNodeId graph)


carveLoops : MazeGraph -> NodeId -> Random.Generator MazeGraph
carveLoops inputGraph startNodeId =
    let
        deadEnds =
            findDeadEnds inputGraph startNodeId

        carveLoop deadEnd ( graph, seed ) =
            let
                targetsGenerator =
                    deadEnd
                        |> QuadGraph.get graph
                        |> Maybe.map disconnectedNodes
                        |> Maybe.withDefault []
                        |> Random.List.shuffle

                ( shuffledTargets, newSeed ) =
                    Random.step targetsGenerator seed

                updatedGraph =
                    shuffledTargets
                        |> List.head
                        |> Maybe.map (\nodeId -> QuadGraph.updateEdge deadEnd nodeId True graph)
                        |> Maybe.withDefault graph
            in
            ( updatedGraph, newSeed )
    in
    Random.int Random.minInt Random.maxInt
        |> Random.map Random.initialSeed
        |> Random.map (Tuple.pair inputGraph)
        |> Random.map (\init -> List.foldl carveLoop init deadEnds)
        |> Random.map Tuple.first


buildRoute : MazeGraph -> Int -> NodeId -> List NodeId
buildRoute graph minAcceptableLength startNodeId =
    let
        vertexDistances =
            QuadGraph.Traverse.foldBreadthFirst
                (\path nodeId distances -> Dict.insert nodeId (nodeId :: path) distances)
                Dict.empty
                (QuadGraph.Foldable identity startNodeId graph)
    in
    vertexDistances
        |> Dict.toList
        |> List.sortBy (Tuple.second >> List.length >> min minAcceptableLength)
        |> List.reverse
        |> List.map Tuple.second
        |> List.filter (\solution -> solution |> List.head |> Maybe.andThen (QuadGraph.get graph >> Maybe.map QuadGraph.isLeaf) |> Maybe.withDefault False)
        |> List.head
        |> Maybe.withDefault []


connectedNodes : MazeGraphNode -> List NodeId
connectedNodes node =
    node
        |> QuadGraph.nodeEdges
        |> List.filterMap (QuadGraph.resolveEdgeIf identity)


disconnectedNodes : MazeGraphNode -> List NodeId
disconnectedNodes node =
    node
        |> QuadGraph.nodeEdges
        |> List.filterMap (QuadGraph.resolveEdgeIf (identity >> not))


new : Int -> Int -> Random.Generator MazePanel
new width height =
    let
        grid =
            newGrid width height
    in
    randomStartPoint grid.graph
        |> Random.andThen
            (\startNodeId ->
                carvePaths grid.graph startNodeId
                    |> Random.andThen
                        (\carvedGraph ->
                            carveLoops carvedGraph startNodeId
                                |> Random.map
                                    (\loopedGraph ->
                                        let
                                            solution =
                                                buildRoute loopedGraph (grid.width * 3) startNodeId

                                            endNodeId =
                                                List.head solution |> Maybe.withDefault -1
                                        in
                                        { start = startNodeId
                                        , end = endNodeId
                                        , optimalSolution = List.reverse solution
                                        , playerPath = [ startNodeId ]
                                        , finishingMove = endNodeId |> QuadGraph.get loopedGraph |> Maybe.andThen (QuadGraph.stubs >> List.head) |> Maybe.withDefault North
                                        , grid = { grid | graph = loopedGraph }
                                        }
                                    )
                        )
            )
