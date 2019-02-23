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
        gameObjects =
            case model.gameState of
                Playing _ game ->
                    Renderer.objects (Renderer.getDimensions game.grid) model

                Completed _ game ->
                    Renderer.objects (Renderer.getDimensions game.grid) model

                _ ->
                    []
    in
    Renderer.board gameObjects


subscriptions _ =
    Time.every 200.0 AiMove


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        ( updatedGame, cmd ) =
            Game.update message model
    in
    ( { updatedGame | level = 4 }, cmd )
