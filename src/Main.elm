module Main exposing (main)

import Browser
import Game
import Renderer


main =
    Browser.element { init = Game.new, update = Game.update, subscriptions = Game.subscriptions, view = Renderer.render }
