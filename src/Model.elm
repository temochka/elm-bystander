module Model exposing (Animation(..), GameState(..), Level, Model, Msg(..), Player(..))

import MazePanel exposing (MazePanel)
import QuadGraph
import Random
import Time


type Player
    = Ai
    | Player


type alias Level =
    Int


type GameState
    = Loading
    | Playing Player MazePanel
    | Completed Player MazePanel


type Animation
    = BlinkPath String


type alias Model =
    { gameState : GameState
    , animations : List Animation
    , player : Player
    , level : Level
    }



-- UPDATE


type Msg
    = SetGame MazePanel
    | KeyPress (Maybe QuadGraph.Direction)
    | AiMove Time.Posix
    | NewGame
    | NextLevel
    | PressedEnter
    | Nop
