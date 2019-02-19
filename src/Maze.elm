module Maze exposing (AdjacencyRecord, Connection(..), Direction(..), Edge(..), Game, Grid, VertexId, VertexOnGrid, edges, getAdjacencyRecord, getDirection, go, makeGame, new, passConnectionIf, vertexIdOnGrid, vertices)

import Dict exposing (Dict)
import Random
import Random.Extra
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
                    Just (Broken north)

                else
                    Nothing
            , east =
                if j < width - 1 then
                    Just (Broken east)

                else
                    Nothing
            , south =
                if i < height - 1 then
                    Just (Broken south)

                else
                    Nothing
            , west =
                if j > 0 then
                    Just (Broken west)

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


passConnectionIf : (VertexId -> Connection) -> Connection -> Maybe VertexId
passConnectionIf kind connection =
    let
        targetVertexId =
            resolveConnection connection
    in
    if connection == kind targetVertexId then
        Just targetVertexId

    else
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


allDirections : List (AdjacencyRecord -> Maybe Connection)
allDirections =
    [ .north, .east, .south, .west ]


availableDirections : AdjacencyRecord -> List VertexId
availableDirections record =
    allDirections
        |> List.filterMap (\method -> method record |> Maybe.map resolveConnection)


passableDirections : AdjacencyRecord -> List VertexId
passableDirections record =
    allDirections
        |> List.filterMap (\method -> method record |> Maybe.andThen (passConnectionIf Intact))


impassableDirections : AdjacencyRecord -> List VertexId
impassableDirections record =
    allDirections
        |> List.filterMap (\method -> method record |> Maybe.andThen (passConnectionIf Broken))


isDisconnected : AdjacencyRecord -> Bool
isDisconnected record =
    record |> passableDirections |> List.isEmpty


atBorder : AdjacencyRecord -> Bool
atBorder record =
    allDirections
        |> List.any (\method -> Nothing == method record)


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


disconnectVertices : VertexId -> VertexId -> AdjacencyList -> AdjacencyList
disconnectVertices vertexIdA vertexIdB adjacencyList =
    let
        helper vertexId record =
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
        |> Dict.update vertexIdA (Maybe.map (helper vertexIdB))
        |> Dict.update vertexIdB (Maybe.map (helper vertexIdA))


connectVertices : VertexId -> VertexId -> AdjacencyList -> AdjacencyList
connectVertices vertexIdA vertexIdB adjacencyList =
    let
        helper vertexId record =
            if record.north == Just (Broken vertexId) then
                { record | north = Just (Intact vertexId) }

            else if record.east == Just (Broken vertexId) then
                { record | east = Just (Intact vertexId) }

            else if record.south == Just (Broken vertexId) then
                { record | south = Just (Intact vertexId) }

            else if record.west == Just (Broken vertexId) then
                { record | west = Just (Intact vertexId) }

            else
                record
    in
    adjacencyList
        |> Dict.update vertexIdA (Maybe.map (helper vertexIdB))
        |> Dict.update vertexIdB (Maybe.map (helper vertexIdA))


type alias GeneratorState =
    { adjacencyList : AdjacencyList
    , currentSeed : Random.Seed
    }


carvePaths : VertexId -> VertexId -> State GeneratorState ()
carvePaths fromVertexId vertexId =
    let
        unlessClaimed f ({ adjacencyList } as state) =
            let
                claimed =
                    getAdjacencyRecord vertexId adjacencyList |> Maybe.map isDisconnected |> Maybe.withDefault True |> not
            in
            if claimed then
                State.get |> State.map (always ())

            else
                State.get |> State.andThen f

        carvePath ({ adjacencyList, currentSeed } as state) =
            let
                updatedAdjacencyList =
                    connectVertices fromVertexId vertexId adjacencyList

                generator =
                    updatedAdjacencyList
                        |> getAdjacencyRecord vertexId
                        |> Maybe.map availableDirections
                        |> Maybe.withDefault []
                        |> Random.List.shuffle

                ( directions, newSeed ) =
                    Random.step generator currentSeed
            in
            State.put { state | currentSeed = newSeed, adjacencyList = updatedAdjacencyList }
                |> State.andThen (\_ -> State.traverse (carvePaths vertexId) directions |> State.map (always ()))
    in
    State.get
        |> State.andThen (unlessClaimed carvePath)
        |> State.map (always ())


buildRoute : AdjacencyList -> Int -> VertexId -> Maybe (List VertexId)
buildRoute adjacencyList minAcceptableLength startVertexId =
    let
        bfs : Int -> List VertexId -> State (Dict VertexId (List VertexId)) (List (List VertexId))
        bfs depth path =
            let
                vertexId =
                    path |> List.head |> Maybe.withDefault -1

                neighbors =
                    adjacencyList |> getAdjacencyRecord vertexId |> Maybe.map passableDirections |> Maybe.withDefault [] |> List.map (\v -> v :: path)

                update distances =
                    if Dict.member vertexId distances then
                        State.put distances |> State.map (always [])

                    else
                        State.put (Dict.insert vertexId path distances) |> State.map (always neighbors)
            in
            State.get |> State.andThen update

        runner : Int -> Dict VertexId (List VertexId) -> List (List VertexId) -> Dict VertexId (List VertexId)
        runner depth distances queue =
            case queue of
                [] ->
                    distances

                _ ->
                    queue
                        |> State.traverse (bfs depth)
                        |> State.run distances
                        |> (\( nextVertices, newDistances ) -> runner (depth + 1) newDistances (List.foldl (++) [] nextVertices))

        vertexDistances =
            runner 0 Dict.empty [ [ startVertexId ] ]
    in
    vertexDistances
        |> Dict.toList
        |> List.sortBy (Tuple.second >> List.length >> min minAcceptableLength)
        |> List.reverse
        |> List.map Tuple.second
        |> List.filter (\solution -> solution |> List.head |> Maybe.andThen (\v -> getAdjacencyRecord v adjacencyList) |> Maybe.map atBorder |> Maybe.withDefault False)
        |> List.head


carveLoops : GeneratorState -> VertexId -> GeneratorState
carveLoops ({ adjacencyList } as initialState) startVertexId =
    let
        findDeadEnds depth sourceVertexId vertexId =
            let
                directions =
                    adjacencyList
                        |> getAdjacencyRecord vertexId
                        |> Maybe.map passableDirections
                        |> Maybe.withDefault []
                        |> List.filter ((/=) sourceVertexId)
            in
            if List.isEmpty directions then
                [ ( depth, vertexId ) ]

            else
                List.concatMap (findDeadEnds (depth + 1) vertexId) directions

        deadEnds =
            findDeadEnds 0 -1 startVertexId

        carveLoop ( deadEnd, depth ) state =
            let
                targetsGenerator =
                    state.adjacencyList
                        |> getAdjacencyRecord deadEnd
                        |> Maybe.map impassableDirections
                        |> Maybe.withDefault []
                        |> Random.List.shuffle

                ( shuffledTargets, newSeed ) =
                    Random.step targetsGenerator state.currentSeed

                updatedList =
                    shuffledTargets
                        |> List.head
                        |> Maybe.map (\vertexId -> connectVertices deadEnd vertexId state.adjacencyList)
                        |> Maybe.withDefault state.adjacencyList
            in
            { state | adjacencyList = updatedList, currentSeed = newSeed }
    in
    deadEnds
        |> List.foldl carveLoop initialState


makeGame : Grid -> Random.Generator ( Grid, Game )
makeGame grid =
    let
        builder state =
            let
                { adjacencyList, currentSeed } =
                    carvePaths -1 state.start |> State.finalState { adjacencyList = state.adjacencyList, currentSeed = state.seed }
            in
            { state | adjacencyList = adjacencyList, seed = currentSeed }

        looper state =
            let
                { adjacencyList, currentSeed } =
                    carveLoops { adjacencyList = state.adjacencyList, currentSeed = state.seed } state.start
            in
            { state | adjacencyList = adjacencyList, seed = currentSeed }

        solver state =
            let
                solution =
                    buildRoute state.adjacencyList (grid.width * 3) state.start |> Maybe.withDefault []
            in
            { state | solution = solution }

        packer state =
            let
                end =
                    List.head state.solution |> Maybe.withDefault -1
            in
            ( { grid | adjacencyList = state.adjacencyList }
            , { start = state.start
              , end = end
              , correctPath = List.reverse state.solution
              , path = [ state.start ]
              , finishingMove = state.adjacencyList |> Dict.get end |> Maybe.andThen missingDirection |> Maybe.withDefault North
              }
            )
    in
    Random.int 42 99999999999
        |> Random.map2 (\start seed -> { solution = [], adjacencyList = grid.adjacencyList, start = start, seed = Random.initialSeed seed })
            (randomStartPoint grid.adjacencyList)
        |> Random.map (builder >> looper >> solver >> packer)
