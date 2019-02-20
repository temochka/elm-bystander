module StepFive exposing (main)

import Browser
import Game
import Model exposing (..)
import Renderer
import Time


main =
    Browser.element { init = Game.newAs Ai 5, update = update, subscriptions = subscriptions, view = view }


view model =
    let
        dimensions =
            Renderer.getDimensions model.maze

        objects =
            Renderer.objects dimensions model
    in
    Renderer.board objects


subscriptions _ =
    Time.every 200.0 AiMove


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        ( updatedGame, cmd ) =
            Game.update message model
    in
    ( { updatedGame | level = 4 }, cmd )
