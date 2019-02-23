module StepTwo exposing (main)

import Browser
import Game
import MazePanel exposing (MazePanel)
import QuadGraph
import Random
import Renderer
import Time


type alias Model =
    { grid : MazePanel.Grid
    , start : QuadGraph.NodeId
    }


type Msg
    = SetStartPoint QuadGraph.NodeId
    | RequestStartPoint


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = MazePanel.newGrid 5 5
      , start = 0
      }
    , Cmd.none
    )


view { grid, start } =
    let
        dimensions =
            Renderer.getDimensions grid
    in
    Renderer.renderEdges dimensions Renderer.gridColor (QuadGraph.edges grid.graph)
        ++ Renderer.renderStartNode dimensions start
        |> Renderer.board


update msg model =
    case msg of
        SetStartPoint start ->
            ( { model | start = start }, Cmd.none )

        RequestStartPoint ->
            ( model, Random.generate SetStartPoint (MazePanel.randomStartPoint model.grid.graph) )


subscriptions _ =
    Time.every 1000.0 (always RequestStartPoint)
