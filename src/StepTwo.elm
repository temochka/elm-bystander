module StepTwo exposing (main)

import Browser
import Game
import Maze
import Random
import Renderer
import Time


type alias Model =
    { grid : Maze.Grid
    , start : Maze.VertexId
    }


type Msg
    = SetStartPoint Maze.VertexId
    | RequestStartPoint


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = Maze.new 5 5
      , start = 0
      }
    , Cmd.none
    )


view { grid, start } =
    let
        dimensions =
            Renderer.getDimensions grid
    in
    Renderer.renderEdges dimensions Renderer.gridColor (Maze.edges grid)
        ++ Renderer.renderStartNode dimensions start
        |> Renderer.board


update msg model =
    case msg of
        SetStartPoint start ->
            ( { model | start = start }, Cmd.none )

        RequestStartPoint ->
            ( model, Random.generate SetStartPoint (Maze.randomStartPoint model.grid.adjacencyList) )


subscriptions _ =
    Time.every 1000.0 (always RequestStartPoint)
