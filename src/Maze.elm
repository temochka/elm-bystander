module Maze exposing (AdjacencyRecord, Direction(..), Edge(..), Game, Grid, VertexId, VertexOnGrid, edges, getAdjacencyRecord, getDirection, go, makeGame, new, passConnection, vertexIdOnGrid, vertices)

import Debug
import Dict exposing (Dict)
import Random
import Random.List
import Set exposing (Set)
import State exposing (State)


type alias Grid =
    { adjacencyList : AdjacencyList
    , width : Int
    , height : Int
    }


type alias VertexOnGrid =
    ( Int, Int )


type Edge
    = Edge VertexId VertexId Bool


type alias VertexId =
    Int


type Direction
    = North
    | East
    | South
    | West


type Connection
    = Intact VertexId
    | Broken VertexId


type alias AdjacencyRecord =
    { north : Maybe Connection
    , east : Maybe Connection
    , south : Maybe Connection
    , west : Maybe Connection
    }


type alias Game =
    { start : VertexId
    , end : VertexId
    , correctPath : List VertexId
    , path : List VertexId
    , finishingMove : Direction
    }


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
                    Just (Intact north)

                else
                    Nothing
            , east =
                if j < width - 1 then
                    Just (Intact east)

                else
                    Nothing
            , south =
                if i < height - 1 then
                    Just (Intact south)

                else
                    Nothing
            , west =
                if j > 0 then
                    Just (Intact west)

                else
                    Nothing
            }
    in
    { adjacencyList =
        rows
            |> List.concatMap (\i -> List.map (\j -> ( toVertexId width ( i, j ), adjacencyRecord i j )) cols)
            |> Dict.fromList
    , width = width
    , height = height
    }


passConnection : Connection -> Maybe VertexId
passConnection connection =
    case connection of
        Intact vertexId ->
            Just vertexId

        Broken _ ->
            Nothing


resolveConnection : Connection -> VertexId
resolveConnection connection =
    case connection of
        Intact vertexId ->
            vertexId

        Broken vertexId ->
            vertexId


toVertexId : Int -> VertexOnGrid -> VertexId
toVertexId width ( i, j ) =
    i * width + j


vertexIdOnGrid : Int -> VertexId -> VertexOnGrid
vertexIdOnGrid width vertexId =
    ( vertexId // width, modBy width vertexId )


isIntact : Connection -> Bool
isIntact connection =
    case connection of
        Intact _ ->
            True

        _ ->
            False


vertices : Grid -> List VertexId
vertices { adjacencyList, width } =
    Dict.keys adjacencyList


getAdjacencyRecord =
    Dict.get


edges : Grid -> List Edge
edges { width, height, adjacencyList } =
    let
        vertexEdges vertexId =
            [ getAdjacencyRecord vertexId adjacencyList |> Maybe.andThen .north
            , getAdjacencyRecord vertexId adjacencyList |> Maybe.andThen .east
            , getAdjacencyRecord vertexId adjacencyList |> Maybe.andThen .south
            , getAdjacencyRecord vertexId adjacencyList |> Maybe.andThen .west
            ]
                |> List.filterMap identity
                |> List.map (\connection -> ( ( vertexId, resolveConnection connection ), isIntact connection ))

        reducer ( ( vertexA, vertexB ), intact ) dict =
            if Dict.member ( vertexB, vertexA ) dict then
                dict

            else
                Dict.insert ( vertexA, vertexB ) intact dict

        makeEdge ( ( vertexA, vertexB ), intact ) =
            Edge vertexA vertexB intact
    in
    adjacencyList
        |> Dict.keys
        |> List.concatMap vertexEdges
        |> List.foldl reducer Dict.empty
        |> Dict.toList
        |> List.map makeEdge


go : Grid -> VertexId -> (AdjacencyRecord -> Maybe VertexId) -> Maybe VertexId
go { adjacencyList, width } originVertexId direction =
    getAdjacencyRecord originVertexId adjacencyList |> Maybe.andThen direction


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
                |> List.filter (\v -> getAdjacencyRecord v adjacencyList |> Maybe.map isBorder |> Maybe.withDefault False)
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


missingDirection : AdjacencyRecord -> Maybe Direction
missingDirection record =
    if record.north == Nothing then
        Just North

    else if record.east == Nothing then
        Just East

    else if record.south == Nothing then
        Just South

    else if record.west == Nothing then
        Just West

    else
        Nothing


availableDirections : AdjacencyRecord -> List VertexId
availableDirections record =
    [ .north, .east, .south, .west ]
        |> List.filterMap (\method -> method record |> Maybe.andThen passConnection)


getDirection : AdjacencyRecord -> VertexId -> Maybe Direction
getDirection record targetVertexId =
    if record.north == Just (Intact targetVertexId) then
        Just North

    else if record.east == Just (Intact targetVertexId) then
        Just East

    else if record.south == Just (Intact targetVertexId) then
        Just South

    else if record.west == Just (Intact targetVertexId) then
        Just West

    else
        Nothing


removeEdge : VertexId -> VertexId -> AdjacencyList -> AdjacencyList
removeEdge vertexIdA vertexIdB adjacencyList =
    let
        removeHelper vertexId record =
            if record.north == Just (Intact vertexId) then
                { record | north = Just (Broken vertexId) }

            else if record.east == Just (Intact vertexId) then
                { record | east = Just (Broken vertexId) }

            else if record.south == Just (Intact vertexId) then
                { record | south = Just (Broken vertexId) }

            else if record.west == Just (Intact vertexId) then
                { record | west = Just (Broken vertexId) }

            else
                record
    in
    adjacencyList
        |> Dict.update vertexIdA (Maybe.map (removeHelper vertexIdB))
        |> Dict.update vertexIdB (Maybe.map (removeHelper vertexIdA))


makeGame : Grid -> Random.Generator ( Grid, Game )
makeGame ({ adjacencyList, width } as grid) =
    let
        randomFind : (a -> Random.Generator (Maybe b)) -> List a -> Random.Generator (Maybe b)
        randomFind f list =
            case list of
                x :: xs ->
                    Random.andThen
                        (\result ->
                            case result of
                                Nothing ->
                                    randomFind f xs

                                value ->
                                    Random.constant value
                        )
                        (f x)

                [] ->
                    Random.constant Nothing

        carvePath : List VertexId -> ( VertexId, VertexId ) -> Random.Generator (Maybe (List VertexId))
        carvePath path ( current, end ) =
            if current == end && List.length path >= width * 2 then
                Random.constant (Just (current :: path))

            else if current == end then
                Random.constant Nothing

            else
                getAdjacencyRecord current adjacencyList
                    |> Maybe.map availableDirections
                    |> Maybe.withDefault []
                    |> Random.List.shuffle
                    |> Random.map (List.filter (\direction -> not (List.member direction path)))
                    |> Random.andThen (randomFind (\vertexId -> carvePath (current :: path) ( vertexId, end )))

        carveGap : Dict VertexId VertexId -> List VertexId -> VertexId -> State AdjacencyList ()
        carveGap thePath path vertexId =
            let
                lastVertexId =
                    List.head path |> Maybe.withDefault -1
            in
            if
                Dict.member vertexId thePath
                    && not (getAdjacencyRecord vertexId thePath |> Maybe.map ((==) lastVertexId) |> Maybe.withDefault False)
                    && not (getAdjacencyRecord lastVertexId thePath |> Maybe.map ((==) vertexId) |> Maybe.withDefault False)
            then
                State.modify (removeEdge (path |> List.head |> Maybe.withDefault -1) vertexId)

            else
                State.get
                    |> State.andThen
                        (\state ->
                            state
                                |> getAdjacencyRecord vertexId
                                |> Maybe.map availableDirections
                                |> Maybe.map (List.filter (\neighbor -> not (List.member neighbor path)))
                                |> Maybe.withDefault []
                                |> State.traverse (\neighbor -> carveGap thePath (vertexId :: path) neighbor)
                                |> State.map (always ())
                        )

        carveGaps : List VertexId -> AdjacencyList
        carveGaps path =
            let
                pathSet =
                    List.map2 Tuple.pair path (Maybe.withDefault [] (List.tail path))
                        |> Dict.fromList
            in
            State.traverse (carveGap pathSet []) path
                |> State.finalState adjacencyList
    in
    randomStartPoint adjacencyList
        |> Random.andThen (juxt (randomEndPoint adjacencyList))
        |> Random.andThen (juxt (carvePath []))
        |> Random.andThen (juxt (Random.constant << carveGaps << Maybe.withDefault [] << Tuple.second))
        |> Random.map
            (\( ( ( start, end ), path ), newAdjacencyList ) ->
                ( { grid | adjacencyList = newAdjacencyList }
                , { start = start
                  , end = end
                  , correctPath = path |> Maybe.withDefault [] |> List.reverse
                  , path = [ start ]
                  , finishingMove = adjacencyList |> Dict.get end |> Maybe.andThen missingDirection |> Maybe.withDefault North
                  }
                )
            )
