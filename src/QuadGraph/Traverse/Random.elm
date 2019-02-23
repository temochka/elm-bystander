module QuadGraph.Traverse.Random exposing (runDepthFirst)

import QuadGraph exposing (..)
import Random
import Random.List
import Set exposing (Set)
import State exposing (State)


type alias RunState a e =
    { graph : QuadGraph a e
    , visited : Set NodeId
    , currentSeed : Random.Seed
    }


runDepthFirst : (List NodeId -> NodeId -> QuadGraph a e -> QuadGraph a e) -> Foldable a e -> Random.Generator (QuadGraph a e)
runDepthFirst reducer (Foldable predicate startNodeId inputGraph) =
    let
        depthFirst : List NodeId -> NodeId -> State (RunState a e) ()
        depthFirst path nodeId =
            let
                unlessVisited f ({ graph, visited } as state) =
                    if Set.member nodeId visited then
                        State.get |> State.map (always ())

                    else
                        State.get |> State.andThen f

                search ({ graph, currentSeed, visited } as state) =
                    let
                        updatedQuadGraph =
                            reducer path nodeId graph

                        generator =
                            nodeId
                                |> QuadGraph.get updatedQuadGraph
                                |> Maybe.map nodeEdges
                                |> Maybe.map (List.filterMap (resolveEdgeIf predicate))
                                |> Maybe.withDefault []
                                |> Random.List.shuffle

                        ( directions, newSeed ) =
                            Random.step generator currentSeed
                    in
                    State.put { state | currentSeed = newSeed, graph = updatedQuadGraph, visited = Set.insert nodeId visited }
                        |> State.andThen (\_ -> State.traverse (depthFirst (nodeId :: path)) directions |> State.map (always ()))
            in
            State.get
                |> State.andThen (unlessVisited search)
                |> State.map (always ())
    in
    Random.int Random.minInt Random.maxInt
        |> Random.map (\number -> { graph = inputGraph, visited = Set.empty, currentSeed = Random.initialSeed number })
        |> Random.map (\state -> depthFirst [] startNodeId |> State.finalState state |> .graph)
