import Window
import Http (sendGet, Success, Response)
import Keyboard (arrows)
import JavaScript as JS

import open Arrow
import open Level
import open Pull

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

update = moves |> keepIf isJust (Just Right) |> lift (\(Just m) -> m)

data Output = Mv Move | Subgoal | Goal | Error | None
type State = {lv : Level,
              adv : Int,
              p : Player,
              ot : Output
             }

state0 : State
state0 = State level0 0 (player0 level0) None

stepFun : Move -> State -> State
stepFun m s = if okMove m s then doMove m s else {s|ot <- Error}

state = foldp stepFun state0 update

okMove : Move -> State -> Bool
okMove m {lv, p} = case m of
    Right -> lv.w > p.i +1
                && not (member (p.i+1, p.j) lv.obstacles)
    Left -> p.i > 0
                && not (member (p.i-1, p.j) lv.obstacles)
    Up -> lv.h > p.j +1
                && not (member (p.i, p.j+1) lv.obstacles)
    Down -> p.j > 0
                && not (member (p.i, p.j-1) lv.obstacles)

doMove : Move -> State -> State
doMove m {lv, adv, p} = let p' = case m of
        Right -> {p|i <- p.i + 1, prev <- m}
        Left  -> {p|i <- p.i - 1, prev <- m}
        Up    -> {p|j <- p.j + 1, prev <- m}
        Down  -> {p|j <- p.j - 1, prev <- m}
    in if |(p'.i, p'.j) /= goal lv adv -> -- no level change
               State lv adv p' (Mv m)
          |succ adv /= length lv.seq ->   -- subgoal
               State lv (succ adv) (player0 lv) Subgoal
          |otherwise ->                   -- goal
               let lv' = nth (succ lv.number) levels
               in State lv' 0 (player0 lv') Goal


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
    obstacleForm = square (0.9 * toFloat (lv.side)) |> filled darkCharcoal
                   in map (\c -> grid lv c obstacleForm) lv.obstacles

scene : (Int,Int) -> Time -> State -> Element
scene (w,h) t {lv,adv,p} = let
    side = toFloat lv.side
    center = move (-side * toFloat (lv.w-1) * 0.5, -side * toFloat (lv.h-1) * 0.5)
        in collage w h <|
        [ rect (toFloat w) (toFloat h) |> filled lightCharcoal ] 
          ++ map center (drawSquares lv ++ drawObstacles lv ++ drawArrows lv adv) ++
        [ drawGoal lv adv t |> center
        , drawPlayer lv p t |> center
        ]

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

clock : Signal Time
clock = foldp (+) 0 <| fps 20
main = layers <~ combine
           [ scene <~ Window.dimensions ~ clock ~ state
           , stats ]

port sound : Signal String
port sound = (show . .ot) <~ state

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
