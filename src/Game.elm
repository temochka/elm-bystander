module Game exposing (new, newAs, subscriptions, update)

import Browser.Events as Events
import Html.Events as Events
import Json.Decode as Decode
import MazePanel exposing (MazePanel)
import Model exposing (..)
import QuadGraph
import Random
import Time


cursorKeyDecoder : Decode.Decoder Msg
cursorKeyDecoder =
    Decode.map (KeyPress << toDirection) (Decode.field "key" Decode.string)


toDirection : String -> Maybe QuadGraph.Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Just QuadGraph.West

        "a" ->
            Just QuadGraph.West

        "ArrowRight" ->
            Just QuadGraph.East

        "d" ->
            Just QuadGraph.East

        "ArrowDown" ->
            Just QuadGraph.South

        "s" ->
            Just QuadGraph.South

        "ArrowUp" ->
            Just QuadGraph.North

        "w" ->
            Just QuadGraph.North

        _ ->
            Nothing


menuKeyDecoder : Decode.Decoder Msg
menuKeyDecoder =
    let
        decoder key =
            case key of
                "Enter" ->
                    PressedEnter

                "r" ->
                    NewGame

                "+" ->
                    NextLevel

                _ ->
                    Nop
    in
    Decode.map decoder (Decode.field "key" Decode.string)


nextAiMove : MazePanel -> Maybe QuadGraph.Direction
nextAiMove ({ grid } as game) =
    let
        currentNode =
            List.head game.playerPath |> Maybe.withDefault game.start

        nextNode =
            game.optimalSolution |> List.drop (max (List.length game.playerPath) 1) |> List.head
    in
    case nextNode of
        Nothing ->
            Just game.finishingMove

        Just nextNodeId ->
            currentNode
                |> QuadGraph.get grid.graph
                |> Maybe.andThen (\node -> QuadGraph.getDirection node nextNodeId)


handleMove : Model -> MazePanel -> Maybe QuadGraph.Direction -> Model
handleMove ({ player } as model) game direction =
    case game.playerPath of
        currentVertex :: previousVertexes ->
            let
                potentialNextVertex =
                    currentVertex
                        |> QuadGraph.get game.grid.graph
                        |> Maybe.map2 (QuadGraph.takeDirectionIf identity) direction
                        |> Maybe.andThen identity

                previousVertex =
                    List.head previousVertexes

                gameOver =
                    currentVertex == game.end && (direction |> Maybe.map ((==) game.finishingMove) |> Maybe.withDefault False)
            in
            case potentialNextVertex of
                Just nextVertex ->
                    if potentialNextVertex == previousVertex then
                        { model | gameState = Playing player { game | playerPath = List.tail game.playerPath |> Maybe.withDefault [] } }

                    else if List.member nextVertex game.playerPath then
                        { model | animations = [ BlinkPath "#89280E" ] }

                    else
                        { model | gameState = Playing player { game | playerPath = nextVertex :: game.playerPath } }

                Nothing ->
                    if gameOver then
                        { model | gameState = Completed player game }

                    else
                        { model | animations = [ BlinkPath "#89280E" ] }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg modelWithOldAnimations =
    let
        model =
            { modelWithOldAnimations | animations = [] }
    in
    case msg of
        SetGame game ->
            ( { model | gameState = Playing model.player game }, Cmd.none )

        KeyPress direction ->
            case model.gameState of
                Playing Player game ->
                    ( handleMove model game direction, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NextLevel ->
            case model.gameState of
                Playing Player game ->
                    newAs Player (model.level + 1) ()

                Completed Player game ->
                    newAs Player (model.level + 1) ()

                Playing Ai game ->
                    newAs Ai (model.level + 1) ()

                _ ->
                    ( model, Cmd.none )

        PressedEnter ->
            case model.gameState of
                Completed Player game ->
                    update NextLevel model

                _ ->
                    update NewGame model

        AiMove _ ->
            case model.gameState of
                Playing Ai game ->
                    let
                        nextMove =
                            nextAiMove game
                    in
                    ( handleMove model game nextMove, Cmd.none )

                Completed Ai _ ->
                    newAs Ai (model.level + 1) ()

                _ ->
                    ( model, Cmd.none )

        NewGame ->
            newAs Player 2 ()

        Nop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown cursorKeyDecoder
        , Events.onKeyDown menuKeyDecoder
        , Time.every 300.0 AiMove
        ]


newAs : Player -> Level -> () -> ( Model, Cmd Msg )
newAs player level _ =
    ( { gameState = Loading, animations = [], level = level, player = player }, Random.generate SetGame (MazePanel.new level level) )


new =
    newAs Ai 2
