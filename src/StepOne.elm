module StepOne exposing (main)

import Browser
import Game
import Maze
import Renderer


main =
    Browser.sandbox { init = Maze.new 5 5, update = always, view = view }


view maze =
    let
        dimensions =
            Renderer.getDimensions maze
    in
    Renderer.renderEdges dimensions Renderer.gridColor (Maze.edges maze)
        |> Renderer.board
