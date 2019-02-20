module StepFour exposing (main)

import Browser
import Game
import Html exposing (Html)
import Maze
import Random
import Renderer
import State
import Time


type alias Model =
    { grid : Maze.Grid
    , carvedPaths : List (List Maze.VertexId)
    , loops : List Maze.Edge
    , start : Maze.VertexId
    , step : Int
    , maxStep : Int
    }


type Msg
    = SetStartPoint Maze.VertexId
    | SetCarvedPaths ( List (List Maze.VertexId), List Maze.Edge )
    | Carve
    | RequestStartPoint


toPaths : List Maze.VertexId -> Maze.VertexId -> Maze.VertexId -> Maze.AdjacencyList -> List (List Maze.VertexId)
toPaths path fromVertexId vertexId adjacencyList =
    let
        children =
            adjacencyList
                |> Maze.getAdjacencyRecord vertexId
                |> Maybe.map Maze.passableDirections
                |> Maybe.withDefault []
                |> List.filter ((/=) fromVertexId)
    in
    if List.isEmpty children then
        [ path ]

    else
        children
            |> List.map (\v -> toPaths (v :: path) vertexId v adjacencyList)
            |> List.concat


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = Maze.new 5 5
      , carvedPaths = []
      , loops = []
      , start = 0
      , step = 10
      , maxStep = 0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view { grid, start, carvedPaths, loops, step, maxStep } =
    let
        dimensions =
            Renderer.getDimensions grid

        paths =
            carvedPaths
                |> List.map (List.take step >> List.reverse)
                |> List.map (Renderer.pathOnGrid dimensions)
                |> List.concatMap (Renderer.renderPath dimensions Renderer.pathColor Nothing)

        loopEdges =
            if step >= maxStep then
                Renderer.renderEdges dimensions "green" loops

            else
                []
    in
    Renderer.renderEdges dimensions Renderer.gridColor (Maze.edges grid)
        ++ loopEdges
        ++ Renderer.renderStartNode dimensions start
        ++ paths
        |> Renderer.board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCarvedPaths ( paths, loops ) ->
            let
                maxStep =
                    paths |> List.map List.length |> List.maximum |> Maybe.withDefault 0
            in
            ( { model | loops = loops, carvedPaths = List.map List.reverse paths, step = 0, maxStep = maxStep }, Cmd.none )

        SetStartPoint start ->
            let
                carver seed =
                    start
                        |> Maze.carvePaths -1
                        |> State.finalState { adjacencyList = model.grid.adjacencyList, currentSeed = seed }

                looper carverResult =
                    let
                        grid =
                            model.grid

                        looperResult =
                            Maze.carveLoops { adjacencyList = carverResult.adjacencyList, currentSeed = carverResult.currentSeed } start

                        carverEdges =
                            Maze.edges { grid | adjacencyList = carverResult.adjacencyList }

                        looperEdges =
                            Maze.edges { grid | adjacencyList = looperResult.adjacencyList }

                        loops =
                            List.filter (\x -> not <| List.member x carverEdges) looperEdges

                        paths =
                            toPaths [ start ] -1 start carverResult.adjacencyList
                    in
                    ( paths, loops )

                generator =
                    Random.int 0 999999
                        |> Random.map Random.initialSeed
                        |> Random.map (carver >> looper)
            in
            ( { model | start = start }, Random.generate SetCarvedPaths generator )

        RequestStartPoint ->
            ( model, Random.generate SetStartPoint (Maze.randomStartPoint model.grid.adjacencyList) )

        Carve ->
            if model.step >= model.maxStep + 10 then
                update RequestStartPoint model

            else
                ( { model | step = model.step + 1 }, Cmd.none )


subscriptions _ =
    Time.every 300.0 (always Carve)
