import Window
import Http (sendGet, Success, Response)
import Keyboard (arrows)
import JavaScript as JS

import open Arrow
import open Level

type Frac = Float -- semantically between 0 and 1
type Movement = Maybe (Move, Frac)

moves : Signal (Maybe Move)
moves = let
    toMove : {x:Int, y:Int} -> Maybe Move
    toMove {x,y} = case (x,y) of
                    (-1, 0) -> Just Left
                    ( 1, 0) -> Just Right
                    ( 0,-1) -> Just Down
                    ( 0, 1) -> Just Up
                    _ -> Nothing
        in lift toMove arrows |> keepIf isJust Nothing

movement : Signal Movement
movement = let toMovement m = case m of
                    Just d -> Just (d, 1)
                    Nothing -> Nothing
           in toMovement <~ moves

type Player = {i : Int, j : Int,
               dx : Float, dy : Float
              }

playerBase (i,j) = Player i j 0 0

update = moves |> keepIf isJust (Just Right) |> lift (\(Just m) -> m)

data Output = Mv Move | Subgoal | Goal | Nada
type State = {lv : Level,
              adv : Int,
              p : Player,
              ot : Output
             }

state0 : State
state0 = State level0 0 (playerBase level0.start) Nada

stepFun : Move -> State -> State
stepFun m s = if okMove m s then doMove m s else {s|ot <- Nada}

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
        Right -> {p|i <- p.i + 1}
        Left  -> {p|i <- p.i - 1}
        Up    -> {p|j <- p.j + 1}
        Down  -> {p|j <- p.j - 1}
    in if |(p'.i, p'.j) /= goal lv adv -> -- no level change
               State lv adv p' (Mv m)
          |succ adv /= length lv.seq ->   -- subgoal
               State lv (succ adv) (playerBase lv.start) Subgoal
          |otherwise ->                   -- goal
               let lv' = nth (succ lv.number) levels
               in State lv' 0 (playerBase lv'.start) Goal


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
drawPlayer lv p t = square (30 + 5*sin (t/200))
                    |> filled black
                    |> rotate (degrees 45)
                    |> grid lv (p.i, p.j)

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
          ++ map center (drawSquares lv)
          ++ map center (drawObstacles lv)
          ++ map center (drawArrows lv adv) ++
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
    ]

main = layers <~ combine
           [ scene <~ Window.dimensions ~ foldp (+) 0 (fps 20) ~ state
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
