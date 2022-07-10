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


type alias Paddle =
    { position : { x : Number, y : Number }
    , velocity : Number
    }


type alias Ball =
    { position : ( Number, Number )
    , velocity : ( Number, Number )
    }


type alias Box =
    { x : Number, y : Number, w : Number, h : Number }


main =
    game view
        update
        { paddleA =
            { position = { x = 0, y = 400 }
            , velocity = 0
            }
        , paddleB =
            { position = { x = 0, y = -400 }
            , velocity = 0
            }
        , ball =
            { position = ( 0, 0 )
            , velocity = ( -ballSpeed, -ballSpeed )
            }
        }


view computer ({ paddleA, paddleB, ball } as memory) =
    let
        w =
            computer.screen.width

        h =
            computer.screen.height

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
    ]


update computer ({ paddleA, paddleB, ball } as memory) =
    let
        ( bx, by ) =
            ball.position

        ( vx, vy ) =
            ball.velocity
    in
    { memory
        | paddleA = { paddleA | velocity = toX computer.keyboard * paddleSpeed } |> move_paddle computer
        , paddleB = { paddleB | velocity = toY computer.keyboard * paddleSpeed } |> move_paddle computer
        , ball =
            { position = clamp_to_screen computer ( bx + vx, by + vy ), velocity = ball.velocity }
                |> handle_wall_collision computer.screen
                |> handle_paddle_collision paddleA
                |> handle_paddle_collision paddleB
    }


move_paddle : Computer -> Paddle -> Paddle
move_paddle computer paddle =
    { paddle | position = { x = clamp_to_screen_width computer (paddle.position.x + paddle.velocity), y = paddle.position.y } }


handle_paddle_collision : Paddle -> Ball -> Ball
handle_paddle_collision paddle ball =
    let
        ( vx, vy ) =
            ball.velocity

        _ =
            Debug.log "Paddle velocity" paddle.velocity

        paddle_box =
            { x = paddle.position.x, y = paddle.position.y, w = paddleWidth, h = paddleHeight }
    in
    if check_rect_collision paddle_box (ball_box ball) then
        { ball | velocity = lift_ball paddle.velocity ( vx, -vy ) }

    else
        ball


lift_ball : Number -> ( Number, Number ) -> ( Number, Number )
lift_ball paddle_velocity ( vx, vy ) =
    ( abs ballSpeed * sign_of paddle_velocity, vy )


handle_wall_collision : Screen -> Ball -> Ball
handle_wall_collision screen ball =
    let
        ( vx, vy ) =
            ball.velocity

        ( x, y ) =
            ball.position
    in
    if x <= screen.left || x >= screen.right then
        { ball | velocity = ( -vx, vy ) }

    else if y <= screen.bottom || y >= screen.top then
        { ball | velocity = ( vx, -vy ) }
        -- Game Over case

    else
        ball


ball_box : Ball -> Box
ball_box ball =
    { x = first ball.position, y = second ball.position, w = ballSize, h = ballSize }


check_rect_collision : Box -> Box -> Bool
check_rect_collision a b =
    a.x < b.x + b.w && a.x + a.w > b.x && a.y < b.y + b.h && a.h + a.y > b.y


clamp_to_screen_width : Computer -> Number -> Number
clamp_to_screen_width computer value =
    clamp -(computer.screen.width / 2) (computer.screen.width / 2) value


clamp_to_screen_height : Computer -> Number -> Number
clamp_to_screen_height computer value =
    clamp -(computer.screen.height / 2) (computer.screen.height / 2) value


clamp_to_screen : Computer -> ( Number, Number ) -> ( Number, Number )
clamp_to_screen computer ( x, y ) =
    ( clamp_to_screen_width computer x, clamp_to_screen_height computer y )


vec_to_tuple : { x : Number, y : Number } -> ( Number, Number )
vec_to_tuple vec =
    ( vec.x, vec.y )


tuple_to_vec : ( Number, Number ) -> { x : Number, y : Number }
tuple_to_vec tuple =
    { x = first tuple, y = second tuple }


sign_of : Number -> Number
sign_of n =
    if n == 0 then
        0

    else
        n / abs n
