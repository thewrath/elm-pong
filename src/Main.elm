module Main exposing (main)

import Playground exposing (..)
import Tuple exposing (first, second)


paddleSpeed =
    10


paddleWidth =
    120


paddleHeight =
    15


ballSpeed =
    5


ballSize =
    15


lifeSize =
    10


rewardSize =
    40


type alias Paddle =
    { position : { x : Number, y : Number }
    , velocity : Number
    }


type alias Ball =
    { position : ( Number, Number )
    , velocity : ( Number, Number )
    }


type Reward
    = Claimed
    | Unclaimed
        { position : ( Number, Number )
        , value : Int
        }


type alias Box =
    { x : Number, y : Number, w : Number, h : Number }


type alias Memory =
    { lives : Int
    , paddleA : Paddle
    , paddleB : Paddle
    , ball : Ball
    , nextReward : Reward
    , score : Int
    }


type GameState
    = Pause
    | Running Memory
    | Over


main =
    game view update Pause


initialMemory =
    { lives = 3
    , paddleA =
        { position = { x = 0, y = 400 }
        , velocity = 0
        }
    , paddleB =
        { position = { x = 0, y = -400 }
        , velocity = 0
        }
    , ball =
        { position = ( 0, 0 )
        , velocity = ( 0, -ballSpeed )
        }
    , nextReward =
        Claimed
    , score = 0
    }


view computer state =
    let
        w =
            computer.screen.width

        h =
            computer.screen.height
    in
    case state of
        Running ({ paddleA, paddleB, ball, nextReward, lives, score } as memory) ->
            let
                ( bx, by ) =
                    ball.position
            in
            [ rectangle black w h
            , rectangle blue paddleWidth paddleHeight
                |> move paddleA.position.x paddleA.position.y
            , rectangle red paddleWidth paddleHeight
                |> move paddleB.position.x paddleB.position.y
            , square white ballSize
                |> move bx by
            , words white ("Score: " ++ String.fromInt score)
                |> move -100 -10
                |> fromTopRight computer.screen
            ]
                ++ renderLives computer.screen lives
                ++ renderReward nextReward

        Pause ->
            [ rectangle black w h, words white "Pause, press space to start" ]

        Over ->
            [ rectangle black w h, words white "Game Over, press space to start" ]


renderLives : Screen -> Int -> List Shape
renderLives screen lives =
    List.repeat lives (rectangle white lifeSize lifeSize)
        |> List.indexedMap (\i x -> move (lifeSize + (toFloat i * 20)) -lifeSize x |> fromTopLeft screen)


renderReward : Reward -> List Shape
renderReward reward =
    case reward of
        Claimed ->
            []

        Unclaimed r ->
            let
                ( rx, ry ) =
                    r.position
            in
            square yellow rewardSize
                |> move rx ry
                |> List.singleton


update computer state =
    case state of
        Running ({ paddleA, paddleB, ball, nextReward } as memory) ->
            let
                ( bx, by ) =
                    ball.position

                ( vx, vy ) =
                    ball.velocity
            in
            Running
                ({ memory
                    | paddleA = { paddleA | velocity = toX computer.keyboard * -paddleSpeed } |> movePaddle computer
                    , paddleB = { paddleB | velocity = toX computer.keyboard * paddleSpeed } |> movePaddle computer
                    , ball =
                        { position = clampToScreen computer ( bx + vx, by + vy ), velocity = ball.velocity }
                            |> handleWallCollision computer.screen
                            |> handlePaddleCollision paddleA
                            |> handlePaddleCollision paddleB
                    , nextReward = handleRewardCollision ball nextReward
                 }
                    |> handleDestroyBall computer.screen
                    |> generateReward computer.time
                )
                |> handleGameOver

        Pause ->
            if computer.keyboard.space then
                Running initialMemory

            else
                state

        Over ->
            if computer.keyboard.space then
                Running initialMemory

            else
                state


movePaddle : Computer -> Paddle -> Paddle
movePaddle computer paddle =
    { paddle | position = { x = clampToScreenWidth computer (paddle.position.x + paddle.velocity), y = paddle.position.y } }


handleRewardCollision : Ball -> Reward -> Reward
handleRewardCollision ball reward =
    case reward of
        Claimed ->
            reward

        Unclaimed r ->
            if checkRectCollision (rewardBox r) (ballBox ball) then
                Claimed

            else
                reward


handlePaddleCollision : Paddle -> Ball -> Ball
handlePaddleCollision paddle ball =
    let
        ( vx, vy ) =
            ball.velocity
    in
    if checkRectCollision (paddleBox paddle) (ballBox ball) then
        { ball | velocity = liftBall paddle.velocity ( vx, -vy ) }

    else
        ball


liftBall : Number -> ( Number, Number ) -> ( Number, Number )
liftBall paddle_velocity ( vx, vy ) =
    ( abs ballSpeed * signOf paddle_velocity, vy )


handleWallCollision : Screen -> Ball -> Ball
handleWallCollision screen ball =
    let
        ( vx, vy ) =
            ball.velocity

        ( x, y ) =
            ball.position
    in
    if x <= screen.left || x >= screen.right then
        { ball | velocity = ( -vx, vy ) }

    else
        ball


handleGameOver : GameState -> GameState
handleGameOver state =
    case state of
        Running memory ->
            if memory.lives <= 0 then
                Over

            else
                state

        _ ->
            state


handleDestroyBall : Screen -> Memory -> Memory
handleDestroyBall screen ({ ball, lives } as memory) =
    let
        ( x, y ) =
            ball.position
    in
    if y <= screen.bottom || y >= screen.top then
        { memory
            | lives = lives - 1
            , ball =
                { position = ( 0, 0 )
                , velocity = ( 0, -ballSpeed )
                }
        }

    else
        memory


generateReward : Time -> Memory -> Memory
generateReward time ({ nextReward } as memory) =
    case nextReward of
        Unclaimed _ ->
            memory

        Claimed ->
            { memory
                | nextReward =
                    Unclaimed
                        { position = ( toFloat (fakeRandomInt time 0 100), toFloat (fakeRandomInt time 0 100) )
                        , value = 10
                        }
            }


ballBox : Ball -> Box
ballBox ball =
    { x = first ball.position - ballSize / 2, y = second ball.position - ballSize / 2, w = ballSize, h = ballSize }


paddleBox : Paddle -> Box
paddleBox paddle =
    { x = paddle.position.x - paddleWidth / 2, y = paddle.position.y - paddleHeight / 2, w = paddleWidth, h = paddleHeight }


rewardBox : { position : ( Number, Number ), value : Int } -> Box
rewardBox reward =
    { x = first reward.position - rewardSize / 2, y = second reward.position - rewardSize / 2, w = rewardSize, h = rewardSize }


checkRectCollision : Box -> Box -> Bool
checkRectCollision a b =
    a.x < b.x + b.w && a.x + a.w > b.x && a.y < b.y + b.h && a.h + a.y > b.y


clampToScreenWidth : Computer -> Number -> Number
clampToScreenWidth computer value =
    clamp -(computer.screen.width / 2) (computer.screen.width / 2) value


clampToScreenHeight : Computer -> Number -> Number
clampToScreenHeight computer value =
    clamp -(computer.screen.height / 2) (computer.screen.height / 2) value


clampToScreen : Computer -> ( Number, Number ) -> ( Number, Number )
clampToScreen computer ( x, y ) =
    ( clampToScreenWidth computer x, clampToScreenHeight computer y )


vecToTuple : { x : Number, y : Number } -> ( Number, Number )
vecToTuple vec =
    ( vec.x, vec.y )


tupleToVec : ( Number, Number ) -> { x : Number, y : Number }
tupleToVec tuple =
    { x = first tuple, y = second tuple }


signOf : Number -> Number
signOf n =
    if n == 0 then
        0

    else
        n / abs n


fromTopLeft : Screen -> Shape -> Shape
fromTopLeft screen shape =
    shape
        |> move screen.left screen.top


fromTopRight : Screen -> Shape -> Shape
fromTopRight screen shape =
    shape
        |> move screen.right screen.top


fakeRandomInt : Time -> Int -> Int -> Int
fakeRandomInt time a b =
    a + round (1 + cos (spin 0.1 time)) * (b // 2)
