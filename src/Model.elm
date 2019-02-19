module Model exposing (Animation(..), GameState(..), Level, Model, Msg(..), Player(..))

import Maze
import Random
import Time


type Player
    = Ai
    | Player


type alias Level =
    Int


type GameState
    = Loading
    | Playing Player Maze.Game
    | Completed Player Maze.Game


type Animation
    = BlinkPath String


type alias Model =
    { maze : Maze.Grid
    , gameState : GameState
    , animations : List Animation
    , player : Player
    , level : Level
    }



-- UPDATE


type Msg
    = UpdateMaze Maze.Grid
    | SetGame ( Maze.Grid, Maze.Game )
    | KeyPress (Maybe Maze.Direction)
    | AiMove Time.Posix
    | NewGame
    | NextLevel
    | PressedEnter
    | Nop
