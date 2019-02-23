module StepThree exposing (main)

import Browser
import Game
import Html exposing (Html)
import MazePanel exposing (MazePanel)
import QuadGraph
import QuadGraph.Traverse
import Random
import Renderer
import State
import Time


type alias Model =
    { grid : MazePanel.Grid
    , carvedPaths : List (List QuadGraph.NodeId)
    , start : QuadGraph.NodeId
    , step : Int
    , maxStep : Int
    }


type Msg
    = SetStartPoint QuadGraph.NodeId
    | SetCarvedPaths (List (List QuadGraph.NodeId))
    | Carve
    | RequestStartPoint


toPaths : QuadGraph.NodeId -> MazePanel.MazeGraph -> List (List QuadGraph.NodeId)
toPaths startNodeId graph =
    let
        reducer path nodeId acc =
            let
                connectionsCount =
                    nodeId
                        |> QuadGraph.get graph
                        |> Maybe.map MazePanel.connectedNodes
                        |> Maybe.withDefault []
                        |> List.length
            in
            if connectionsCount <= 1 then
                (nodeId :: path) :: acc

            else
                acc
    in
    QuadGraph.Traverse.foldDepthFirst reducer [] (QuadGraph.Foldable identity startNodeId graph)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = MazePanel.newGrid 5 5
      , carvedPaths = []
      , start = 0
      , step = 10
      , maxStep = 0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view { grid, start, carvedPaths, step } =
    let
        dimensions =
            Renderer.getDimensions grid

        paths =
            carvedPaths
                |> List.map (List.take step >> List.reverse)
                |> List.map (Renderer.pathOnGrid dimensions)
                |> List.concatMap (Renderer.renderPath dimensions Renderer.pathColor Nothing)
    in
    Renderer.renderEdges dimensions Renderer.gridColor (QuadGraph.edges grid.graph)
        ++ Renderer.renderStartNode dimensions start
        ++ paths
        |> Renderer.board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCarvedPaths paths ->
            let
                maxStep =
                    paths |> List.map List.length |> List.maximum |> Maybe.withDefault 0
            in
            ( { model | carvedPaths = List.map List.reverse paths, step = 0, maxStep = maxStep }, Cmd.none )

        SetStartPoint start ->
            let
                generator =
                    MazePanel.carvePaths model.grid.graph start
                        |> Random.map (toPaths start)
            in
            ( { model | start = start }, Random.generate SetCarvedPaths generator )

        RequestStartPoint ->
            ( model, Random.generate SetStartPoint (MazePanel.randomStartPoint model.grid.graph) )

        Carve ->
            if model.step >= model.maxStep + 10 then
                update RequestStartPoint model

            else
                ( { model | step = model.step + 1 }, Cmd.none )


subscriptions _ =
    Time.every 300.0 (always Carve)
