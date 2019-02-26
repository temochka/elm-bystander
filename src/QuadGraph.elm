module QuadGraph exposing (Direction(..), Edge(..), Foldable(..), Node, NodeId, QuadGraph, edges, empty, get, getDirection, insertEdge, insertNode, isLeaf, neighbors, nodeEdges, nodeIds, resolveEdgeIf, stubs, takeDirectionIf, updateEdge)

import Dict exposing (Dict)


type alias NodeId =
    Int


type Edge e
    = Edge NodeId NodeId e


type Direction
    = North
    | East
    | South
    | West


type alias Node a e =
    { id : NodeId
    , value : a
    , north : Maybe (Edge e)
    , east : Maybe (Edge e)
    , south : Maybe (Edge e)
    , west : Maybe (Edge e)
    }


type alias QuadGraph a e =
    Dict NodeId (Node a e)


type Foldable a e
    = Foldable (e -> Bool) NodeId (QuadGraph a e)


empty : QuadGraph a e
empty =
    Dict.empty


get : QuadGraph a e -> NodeId -> Maybe (Node a e)
get graph nodeId =
    Dict.get nodeId graph


neighborAccessors : List (Node a e -> Maybe (Edge e))
neighborAccessors =
    [ .north, .east, .south, .west ]


resolveEdge : Edge e -> NodeId
resolveEdge (Edge _ id _) =
    id


resolveEdgeIf : (e -> Bool) -> Edge e -> Maybe NodeId
resolveEdgeIf f (Edge _ id val) =
    if f val then
        Just id

    else
        Nothing


neighbors : Node a e -> List NodeId
neighbors node =
    nodeEdges node |> List.map resolveEdge


nodeIds : QuadGraph a e -> List NodeId
nodeIds graph =
    Dict.keys graph


nodeEdges : Node a e -> List (Edge e)
nodeEdges node =
    neighborAccessors |> List.filterMap ((|>) node)


edges : QuadGraph a e -> List (Edge e)
edges graph =
    let
        toEdges nodeId =
            let
                node =
                    get graph nodeId
            in
            nodeId
                |> get graph
                |> Maybe.map nodeEdges
                |> Maybe.withDefault []
                |> List.map (\(Edge _ id val) -> ( ( nodeId, id ), Edge nodeId id val ))

        reducer ( ( nodeA, nodeB ), edge ) dict =
            if Dict.member ( nodeB, nodeA ) dict then
                dict

            else
                Dict.insert ( nodeA, nodeB ) edge dict
    in
    graph
        |> nodeIds
        |> List.concatMap toEdges
        |> List.foldl reducer Dict.empty
        |> Dict.values



-- A quad graph node is considered a leaf if at least one edge is missing


isLeaf : Node a e -> Bool
isLeaf node =
    List.length (neighbors node) /= List.length neighborAccessors


stubs : Node a e -> List Direction
stubs node =
    [ if node.north == Nothing then
        Just North

      else
        Nothing
    , if node.east == Nothing then
        Just East

      else
        Nothing
    , if node.south == Nothing then
        Just South

      else
        Nothing
    , if node.west == Nothing then
        Just West

      else
        Nothing
    ]
        |> List.filterMap identity


isConnected : Node a e -> (Node a e -> Maybe (Edge e)) -> NodeId -> Bool
isConnected node accessor nodeId =
    case accessor node of
        Just (Edge _ actualNodeId _) ->
            actualNodeId == nodeId

        _ ->
            False


oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        North ->
            South

        East ->
            West

        South ->
            North

        West ->
            East


getDirection : Node a e -> NodeId -> Maybe Direction
getDirection node targetNodeId =
    if isConnected node .north targetNodeId then
        Just North

    else if isConnected node .east targetNodeId then
        Just East

    else if isConnected node .south targetNodeId then
        Just South

    else if isConnected node .west targetNodeId then
        Just West

    else
        Nothing


takeDirectionIf : (e -> Bool) -> Direction -> Node a e -> Maybe NodeId
takeDirectionIf f direction originNode =
    case direction of
        North ->
            originNode.north |> Maybe.andThen (resolveEdgeIf f)

        East ->
            originNode.east |> Maybe.andThen (resolveEdgeIf f)

        South ->
            originNode.south |> Maybe.andThen (resolveEdgeIf f)

        West ->
            originNode.west |> Maybe.andThen (resolveEdgeIf f)


insertNode : NodeId -> a -> QuadGraph a e -> QuadGraph a e
insertNode nodeId value graph =
    let
        node =
            { id = nodeId
            , value = value
            , north = Nothing
            , east = Nothing
            , west = Nothing
            , south = Nothing
            }
    in
    Dict.insert nodeId node graph


insertEdge : Direction -> NodeId -> NodeId -> e -> QuadGraph a e -> QuadGraph a e
insertEdge directionAB nodeIdA nodeIdB val graph =
    let
        helper direction nodeId node =
            let
                edge =
                    Edge node.id nodeId val
            in
            case direction of
                North ->
                    { node | north = Just edge }

                East ->
                    { node | east = Just edge }

                South ->
                    { node | south = Just edge }

                West ->
                    { node | west = Just edge }
    in
    graph
        |> Dict.update nodeIdA (Maybe.map (helper directionAB nodeIdB))
        |> Dict.update nodeIdB (Maybe.map (helper (oppositeDirection directionAB) nodeIdA))


updateEdge : NodeId -> NodeId -> e -> QuadGraph a e -> QuadGraph a e
updateEdge nodeIdA nodeIdB val graph =
    let
        helper nodeId node =
            if isConnected node .north nodeId then
                { node | north = Just (Edge node.id nodeId val) }

            else if isConnected node .east nodeId then
                { node | east = Just (Edge node.id nodeId val) }

            else if isConnected node .south nodeId then
                { node | south = Just (Edge node.id nodeId val) }

            else if isConnected node .west nodeId then
                { node | west = Just (Edge node.id nodeId val) }

            else
                node
    in
    graph
        |> Dict.update nodeIdA (Maybe.map (helper nodeIdB))
        |> Dict.update nodeIdB (Maybe.map (helper nodeIdA))
