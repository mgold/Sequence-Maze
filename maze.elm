import Window
import Http (sendGet, Success, Response)
import Keyboard (arrows)
import JavaScript as JS

import  Arrow (..)
import Level (..)
import Pull (..)

type Frac = Float -- semantically between 0 and 1

moves_keyboard : Signal (Maybe Move)
moves_keyboard = let
    toMove : {x:Int, y:Int} -> Maybe Move
    toMove {x,y} = case (x,y) of
            (-1, 0) -> Just Left
            ( 1, 0) -> Just Right
            ( 0,-1) -> Just Down
            ( 0, 1) -> Just Up
            _ -> Nothing
        in lift toMove arrows |> keepIf isJust Nothing

moves_pull = let
    toMove s = case s of
            Just "Left" -> Just Left
            Just "Right" -> Just Right
            Just "Up" -> Just Up
            Just "Down" -> Just Down
            _ -> Nothing
        in lift toMove pull |> keepIf isJust Nothing

moves = moves_keyboard

type Player = {i : Int, j : Int, prev : Move }

player0 : Level -> Player
player0 lvl = Player (fst lvl.start) (snd lvl.start) lvl.face0

clock : Signal Time
clock = foldp (+) 0 <| fps 20

update : Signal Move
update = keepJusts Right moves

data Output = Mv Move | Subgoal | Goal | Error | None
type Play = {lv : Level,
             adv : Int,
             p : Player,
             ot : Output
            }
data State = Welcome | Game Play | YouWin Int
movesToResetWin = 3

state0 : State
state0 = Welcome

stepFun : Move -> State -> State
stepFun m s = case s of
    Game p -> if okMove m p then doMove m p else Game {p|ot <- Error}
    Welcome -> Game <| Play level0 0 (player0 level0) None
    YouWin c -> if c + 1 == movesToResetWin then Welcome else YouWin (c+1)

state = foldp stepFun state0 update

okMove : Move -> Play -> Bool
okMove m {lv, p} = case m of
    Right -> lv.w > p.i +1
                && not (member (p.i, p.j) lv.obsRight)
    Left -> p.i > 0
                && not (member (p.i-1, p.j) lv.obsRight)
    Up -> lv.h > p.j +1
                && not (member (p.i, p.j) lv.obsUp)
    Down -> p.j > 0
                && not (member (p.i, p.j-1) lv.obsUp)

doMove : Move -> Play -> State
doMove m {lv, adv, p} = let p' = case m of
        Right -> {p|i <- p.i + 1, prev <- m}
        Left  -> {p|i <- p.i - 1, prev <- m}
        Up    -> {p|j <- p.j + 1, prev <- m}
        Down  -> {p|j <- p.j - 1, prev <- m}
    in if |(p'.i, p'.j) /= goal lv adv -> -- no level change
               Game <| Play lv adv p' (Mv m)
          |succ adv /= length lv.seq ->   -- subgoal
               Game <| Play lv (succ adv) (player0 lv) Subgoal
          |otherwise ->                   -- goal
               maybe (YouWin 0) (\lv' -> Game <| Play lv' 0 (player0 lv') Goal)
                    <| nth (succ lv.number) levels

-- DRAW ----
grid : Level -> (Int, Int) -> Form -> Form
grid lv pos = move <| both toFloat <| both ((*) lv.side) <| pos

drawArrows : Level -> Int -> [Form]
drawArrows lv adv = take (succ adv) lv.seq |> concat
                      |> map (\(c, m) -> grid lv c <| arrowForm m)

drawSquares : Level -> [Form]
drawSquares lv = map (\p -> square (0.99*toFloat lv.side)
                            |> filled white |> grid lv p)
   <| pairs [0..pred lv.w] [0..pred lv.h]

drawPlayer : Level -> Player -> Time -> Form
drawPlayer lv p t = let
    face : Move -> Form -> Form
    face m = case m of
        Up -> moveY 10
        Down -> moveY -10
        Left -> moveX -10
        Right -> moveX 10
                    in group
    [ square (30 + 5*sin (t/200))
        |> filled black
        |> rotate (degrees 45)
        |> grid lv (p.i, p.j)
    , circle 5
        |> filled yellow
        |> grid lv (p.i, p.j)
        |> face p.prev
    ]

drawGoal : Level -> Int -> Float -> Form
drawGoal lv adv t = square (if isSubgoal lv adv then 25 else 50)
                    |> filled yellow
                    |> rotate (turns <| t/3000)
                    |> grid lv (goal lv adv)

drawObstacles : Level -> [Form]
drawObstacles lv = let
    side = toFloat lv.side
    obstacleFormV = rect (0.1 * side) (1.1 * side)
                        |> filled darkCharcoal
                        |> moveX (0.5*side)
    obstacleFormH = rect (1.1 * side) (0.1 * side)
                        |> filled darkCharcoal
                        |> moveY (0.5*side)
                   in map (\c -> grid lv c obstacleFormV) lv.obsRight
                   ++ map (\c -> grid lv c obstacleFormH) lv.obsUp

scene : (Int,Int) -> Time -> State -> Element
scene (w,h) t s = case s of
    Welcome -> asText "Welcome!"
    YouWin _ -> asText "You Win!"
    Game {lv,adv,p} -> let
        side = toFloat lv.side
        center = move (-side * toFloat (lv.w-1) * 0.5, -side * toFloat (lv.h-1) * 0.5)
            in collage w h <|
            [ rect (toFloat w) (toFloat h) |> filled lightCharcoal ] 
              ++ map center (drawSquares lv ++ drawObstacles lv ++ drawArrows lv adv) ++
            [ drawGoal lv adv t |> center
            , drawPlayer lv p t |> center
            ]

{-
stats : Signal Element
stats = flow <~ constant down ~ combine
    [ (\lv -> (text . monospace . toText) (show lv.number++": "++show lv.w++"x"++show lv.h++" @"++show lv.side))
        <~ lift .lv state
    , asText <~ arrows
    , asText <~ moves
    , asText <~ lift .adv state
    , asText <~ lift .ot state
    , (\p -> (text . monospace . toText) ("("++show p.i++","++show p.j++")"))
        <~ lift .p state
    , lift asText <| (\{lv, adv} -> goal lv adv) <~ state
    , lift asText pull
    ]
-}
stats = constant empty

main = layers <~ combine
           [ scene <~ Window.dimensions ~ clock ~ state
           , stats ]

--port sound : Signal String
--port sound = (show . .ot) <~ state

-- Below: should be library functions

movePolar : (Float, Float) -> Form -> Form
movePolar (r, theta) = move <| fromPolar (r, theta)

both : (a -> b) -> (a, a) -> (b, b)
both f (m, n) = (f m, f n)

-- because Elm doesn't have list comprehensions
pairs xs ys = let n = length ys in case xs of
    [] -> []
    (x::tl) -> (zip (map (always x) [1..n]) ys) ++ pairs tl ys

pred = flip (-) <| 1

member : a -> [a] -> Bool
member findMe list = case list of
    [] -> False
    (x::xs) -> if x == findMe then True else member findMe xs

keepJusts : a -> Signal (Maybe a) -> Signal a
keepJusts a sa = lift (maybe a id) (keepIf isJust Nothing sa)
