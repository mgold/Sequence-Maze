import Window
import Http (sendGet, Success, Response)
import Keyboard (arrows)
import JavaScript as JS

import open Arrow

type Frac = Float -- semantically between 0 and 1
type Movement = Maybe (Move, Frac)
type Coord = (Int, Int)

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

type Sequence = [(Coord, Move)]

type Level = {number: Int,
              side: Int,
              w: Int,
              h: Int,
              start: Coord,
              goal : Coord,
              obstacles: [Coord],
              seq: [Sequence]
              }

isSubgoal : Level -> Int -> Bool
isSubgoal lv adv = succ adv < length lv.seq

goal : Level -> Int -> Coord
goal lv adv = if isSubgoal lv adv
              then lv.seq |> nth (succ adv) |> head |> fst
              else lv.goal

levels : [Level]
levels = [
    Level 0 150 4 3 (1,0) (3,0) [(1,1), (2,1), (2,0)]
    [ [((1,0), Left), ((0,0), Up), ((0,1), Up)]
    , [((0,2), Right), ((1,2), Right), ((2,2), Right)]
    , [((3,2), Down), ((3,1), Down) ]
    ]
    ,
    Level 1 80 3 8 (0,0) (2,7) [(0,1),(1,1),(1,3),(1,4),(1,5),(2,5)]
    [ [((0,0), Right), ((1,0), Right), ((2,0), Up)]
    , [((2,1), Up), ((2,2), Left), ((1,2), Left)]
    , [((0,2), Up), ((0,3), Up), ((0,4), Up), ((0,5), Up)]
    , [((0,6), Right), ((1,6), Right), ((2,6), Up)]
    ]
    ,
    Level 2 120 5 4 (0,0) (4,3) [(1,0),(1,1),(1,2), (3,3),(3,2),(3,1)]
    [ [((0,0), Up), ((0,1), Up), ((0,2), Up)]
    , [((0,3), Right), ((1,3), Right)]
    , [((2,3), Down), ((2,2), Down)]
    , [((2,1), Down), ((2,0), Right), ((3,0), Right), ((4,0), Up)]
    , [((4,1), Up), ((4,2), Up)]
    ]
    ,
    Level 3 80 8 7 (0,0) (6,0)
        [(0,1),(1,3),(1,5),(2,0),(2,1),(2,3),(2,5),(2,6),(3,3),(4,1),(4,2),(4,3),(4,4),(4,5),(5,0),(6,1),(6,2),(6,3),(6,5),(6,6)]
    [ [((0,0),Right),((1,0),Up),((1,1),Up),((1,2),Left)]
    , [((0,2),Up),((0,3),Up),((0,4),Right),((1,4),Right),((2,4),Right)]
    , [((3,4),Up),((3,5),Up),((3,6),Right),((4,6),Right),((5,6),Down),((5,5),Down)]
    , [((5,4),Right),((6,4),Right),((7,4),Down),((7,3),Down),((7,2),Down),((7,1),Down),((7,0),Left)]
    ]
    ,
    Level 4 160 4 1 (0,0) (3,0) [] [[((1,0),Right), ((2,0),Right)]]
    ]

level0 = head levels

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

sound : Signal String
sound = (show . .ot) <~ state

{-
soundJS = lift JS.fromString sound
foreign export jsevent "sound"
    soundJS : Signal JS.JSString
-}

-- Below: should be library functions

movePolar : (Float, Float) -> Form -> Form
movePolar (r, theta) = move <| fromPolar (r, theta)

both : (a -> b) -> (a, a) -> (b, b)
both f (m, n) = (f m, f n)

-- because Elm doesn't have list comprehensions
pairs xs ys = let n = length ys in case xs of
    [] -> []
    (x::tl) -> (zip (map (\_->x) [1..n]) ys) ++ pairs tl ys

succ = (+) 1
pred = flip (-) <| 1

member : a -> [a] -> Bool
member findMe list = case list of
    [] -> False
    (x::xs) -> if x == findMe then True else member findMe xs

nth : Int -> [a] -> a
nth i xs = head <| drop i xs
