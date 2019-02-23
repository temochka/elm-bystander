module StepOne exposing (main)

import Browser
import Game
import MazePanel exposing (MazePanel)
import QuadGraph
import Renderer


main =
    Browser.sandbox { init = MazePanel.newGrid 5 5, update = always, view = view }


view grid =
    let
        dimensions =
            Renderer.getDimensions grid
    in
    Renderer.renderEdges dimensions Renderer.gridColor (QuadGraph.edges grid.graph)
        |> Renderer.board
