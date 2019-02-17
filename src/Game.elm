module Game exposing (new, subscriptions, update)

import Browser.Events as Events
import Html.Events as Events
import Json.Decode as Decode
import Maze
import Model exposing (..)
import Random
import Time


cursorKeyDecoder : Decode.Decoder Msg
cursorKeyDecoder =
    Decode.map (KeyPress << toDirection) (Decode.field "key" Decode.string)


toDirection : String -> Maybe Maze.Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Just Maze.West

        "a" ->
            Just Maze.West

        "ArrowRight" ->
            Just Maze.East

        "d" ->
            Just Maze.East

        "ArrowDown" ->
            Just Maze.South

        "s" ->
            Just Maze.South

        "ArrowUp" ->
            Just Maze.North

        "w" ->
            Just Maze.North

        _ ->
            Nothing


enterKeyDecoder : Decode.Decoder Msg
enterKeyDecoder =
    let
        decodeEnter key =
            if key == "Enter" then
                NewGame

            else
                Nop
    in
    Decode.map decodeEnter (Decode.field "key" Decode.string)


directionToAccessor : Maybe Maze.Direction -> (Maze.AdjacencyRecord -> Maybe Maze.VertexId)
directionToAccessor direction =
    case direction of
        Just Maze.West ->
            \record -> record.west |> Maybe.andThen (Maze.passConnectionIf Maze.Intact)

        Just Maze.East ->
            \record -> record.east |> Maybe.andThen (Maze.passConnectionIf Maze.Intact)

        Just Maze.North ->
            \record -> record.north |> Maybe.andThen (Maze.passConnectionIf Maze.Intact)

        Just Maze.South ->
            \record -> record.south |> Maybe.andThen (Maze.passConnectionIf Maze.Intact)

        Nothing ->
            always Nothing


nextAiMove : Maze.Grid -> Maze.Game -> Maybe Maze.Direction
nextAiMove grid game =
    let
        currentNode =
            List.head game.path |> Maybe.withDefault game.start

        nextNode =
            game.correctPath |> List.drop (max (List.length game.path) 1) |> List.head
    in
    case nextNode of
        Nothing ->
            Just game.finishingMove

        Just node ->
            Maze.getAdjacencyRecord currentNode grid.adjacencyList
                |> Maybe.andThen (\record -> Maze.getDirection record node)


handleMove : Model -> Maze.Game -> Maybe Maze.Direction -> Model
handleMove ({ player, maze } as model) game direction =
    case game.path of
        currentVertex :: previousVertexes ->
            let
                potentialNextVertex =
                    Maze.go maze currentVertex (directionToAccessor direction)

                previousVertex =
                    List.head previousVertexes

                gameOver =
                    currentVertex == game.end && (direction |> Maybe.map ((==) game.finishingMove) |> Maybe.withDefault False)
            in
            case potentialNextVertex of
                Just nextVertex ->
                    if potentialNextVertex == previousVertex then
                        { model | gameState = Playing player { game | path = List.tail game.path |> Maybe.withDefault [] } }

                    else if List.member nextVertex game.path then
                        { model | animations = [ BlinkPath "#89280E" ] }

                    else
                        { model | gameState = Playing player { game | path = nextVertex :: game.path } }

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
        UpdateMaze maze ->
            ( { model | maze = maze }, Cmd.none )

        SetGame ( grid, game ) ->
            ( { model | maze = grid, gameState = Playing model.player game }, Cmd.none )

        KeyPress direction ->
            case model.gameState of
                Playing Player game ->
                    ( handleMove model game direction, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AiMove _ ->
            case model.gameState of
                Playing Ai game ->
                    let
                        nextMove =
                            nextAiMove model.maze game
                    in
                    ( handleMove model game nextMove, Cmd.none )

                Completed Ai _ ->
                    newAs Ai ()

                _ ->
                    ( model, Cmd.none )

        NewGame ->
            newAs Player ()

        Nop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown cursorKeyDecoder
        , Events.onKeyDown enterKeyDecoder
        , Time.every 300.0 AiMove
        ]


newAs : Player -> () -> ( Model, Cmd Msg )
newAs player _ =
    let
        maze =
            Maze.new 6 6
    in
    ( { maze = maze, gameState = Loading, animations = [], player = player }, Random.generate SetGame (Maze.makeGame maze) )


new =
    newAs Ai
