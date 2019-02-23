module QuadGraph.Traverse exposing (foldBreadthFirst, foldDepthFirst)

import QuadGraph exposing (..)
import Random
import Set exposing (Set)
import State exposing (State)


type alias RunState b =
    { visited : Set NodeId
    , userState : b
    }


foldDepthFirst : (List NodeId -> NodeId -> b -> b) -> b -> Foldable a e -> b
foldDepthFirst func acc (Foldable predicate startNodeId graph) =
    let
        depthFirst : List NodeId -> NodeId -> State (RunState b) ()
        depthFirst path nodeId =
            let
                unlessVisited f ({ visited } as state) =
                    if Set.member nodeId visited then
                        State.get |> State.map (always ())

                    else
                        State.get |> State.andThen f

                update state =
                    if Set.member nodeId state.visited then
                        State.put state |> State.map (always ())

                    else
                        let
                            visited =
                                Set.insert nodeId state.visited

                            neighbors =
                                nodeId
                                    |> QuadGraph.get graph
                                    |> Maybe.map nodeEdges
                                    |> Maybe.map (List.filterMap (resolveEdgeIf predicate))
                                    |> Maybe.withDefault []

                            newUserState =
                                func path nodeId state.userState
                        in
                        State.put { state | visited = visited, userState = newUserState }
                            |> State.andThen (\_ -> State.traverse (depthFirst (nodeId :: path)) neighbors |> State.map (always ()))
            in
            State.get
                |> State.andThen (unlessVisited update)
                |> State.map (always ())
    in
    depthFirst [] startNodeId
        |> State.finalState { visited = Set.empty, userState = acc }
        |> .userState


foldBreadthFirst : (List NodeId -> NodeId -> b -> b) -> b -> Foldable a e -> b
foldBreadthFirst func acc (Foldable predicate startNodeId graph) =
    let
        breadthFirst : List NodeId -> NodeId -> State (RunState b) (List (List NodeId))
        breadthFirst path nodeId =
            let
                update state =
                    if Set.member nodeId state.visited then
                        State.put state |> State.map (always [])

                    else
                        let
                            visited =
                                Set.insert nodeId state.visited

                            neighbors =
                                nodeId
                                    |> QuadGraph.get graph
                                    |> Maybe.map nodeEdges
                                    |> Maybe.map (List.filterMap (resolveEdgeIf predicate))
                                    |> Maybe.withDefault []
                                    |> List.map (\n -> n :: nodeId :: path)

                            newUserState =
                                func path nodeId state.userState
                        in
                        State.put { state | visited = visited, userState = newUserState } |> State.map (always neighbors)
            in
            State.get |> State.andThen update

        runner state queue =
            case queue of
                [] ->
                    state

                _ ->
                    queue
                        |> List.filterMap (\path -> Maybe.map2 Tuple.pair (List.tail path) (List.head path))
                        |> State.traverse (\( path, nodeId ) -> breadthFirst path nodeId)
                        |> State.run state
                        |> (\( nextNodes, newState ) -> runner newState (List.foldl (++) [] nextNodes))
    in
    runner { visited = Set.empty, userState = acc } [ [ startNodeId ] ]
        |> .userState
