import Window
import Http (sendGet, Success, Response)
import Keyboard (arrows)

data Move = Up | Down | Left | Right
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

playerForm : Form
playerForm = square 30 |> filled black |> rotate (degrees 45)

subgoalForm : Form
subgoalForm = square 25 |> filled yellow

goalForm : Form
goalForm = square 50 |> filled yellow

arrow = polygon [ (2.357,180.198)
                , (182.555,0)
                , (2.357,-180.196)
                , (-116.492,-180.196)
                , (20.568,-43.131)
                , (-248.765,-43.131)
                , (-248.765,44.091)
                , (19.61,44.091)
                , (-116.707,180.411)
                , (2.357,180.198)
                ]

arrowForm : Move -> Form
arrowForm m = scale 0.15 <| case m of
    Right -> arrow |> filled green
    Up    -> arrow |> filled blue   |> rotate (turns 0.25)
    Left  -> arrow |> filled purple |> rotate (turns 0.5)
    Down  -> arrow |> filled red    |> rotate (turns 0.75)

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

level1 = Level 1 100 5 4 (0,0) (4,3) [(1,0),(1,1),(1,2), (3,3),(3,2),(3,1)]
      [ [((0,0), Up), ((0,1), Up), ((0,2), Up)]
      , [((0,3), Right), ((1,3), Right)]
      , [((2,3), Down), ((2,2), Down)]
      ]

levels : [Level]
levels = [level1]

update = moves |> keepIf isJust (Just Right) |> lift (\(Just m) -> m)

type State = {lv : Level,
              adv : Int,
              p : Player,
              lm : Maybe Move
             }

state0 : State
state0 = State level1 0 (playerBase level1.start) Nothing

stepFun : Move -> State -> State
stepFun m s = if okMove m s then doMove m s else {s|lm <- Nothing}

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
doMove m {lv, adv, p} = let
    p' = case m of
            Right -> {p|i <- p.i + 1}
            Left  -> {p|i <- p.i - 1}
            Up    -> {p|j <- p.j + 1}
            Down  -> {p|j <- p.j - 1}
    (lv', adv') = if (p'.i, p'.j) /= goal lv adv
                  then (lv, adv)
                  else (if succ adv /= length lv.seq
                          then (lv, succ adv)
                          else (nth (succ lv.number) levels, 0))
                        in State lv' adv' p' <| Just m

--Draw
grid : Level -> (Int, Int) -> Form -> Form
grid lv pos = move <| both toFloat <| both ((*) lv.side) <| pos

drawSquares : Level -> [Form]
drawSquares lv = map (\p -> square (0.99*toFloat lv.side)
                            |> filled white |> grid lv p)
   <| pairs [0..pred lv.w] [0..pred lv.h]

drawPlayer : Level -> Player -> Form
drawPlayer lv p = playerForm |> grid lv (p.i, p.j)

drawGoal : Level -> Int -> Form
drawGoal lv adv = (if isSubgoal lv adv
                  then subgoalForm
                  else goalForm) |> grid lv (goal lv adv)

drawArrows : Level -> Int -> [Form]
drawArrows lv adv = take (succ adv) lv.seq |> concat
                      |> map (\(c, m) -> grid lv c <| arrowForm m)

drawObstacles : Level -> [Form]
drawObstacles lv = let
    obstacleForm = square (0.9 * toFloat (lv.side)) |> filled darkCharcoal
                   in map (\c -> grid lv c obstacleForm) lv.obstacles

scene : (Int,Int) -> State -> Element
scene (w,h) {lv,adv,p} = let
    side = toFloat lv.side
    center = move (-side * toFloat (lv.w-1) * 0.5, -side * toFloat (lv.h-1) * 0.5)
        in collage w h <|
        [ rect (toFloat w) (toFloat h) |> filled lightCharcoal ] 
          ++ map center (drawSquares lv)
          ++ map center (drawObstacles lv)
          ++ map center (drawArrows lv adv) ++
        [ drawGoal lv adv  |> center
        , drawPlayer lv p |> center
        ]

stats : Signal Element
stats = flow <~ constant down ~ combine
    [ (\lv -> (text . monospace . toText) (show lv.number++": "++show lv.w++"x"++show lv.h++" @"++show lv.side))
        <~ lift .lv state
    , asText <~ arrows
    , asText <~ moves
    , asText <~ lift .adv state
    , asText <~ lift .lm state
    , (\p -> (text . monospace . toText) ("("++show p.i++","++show p.j++")"))
        <~ lift .p state
    , lift asText <| (\{lv, adv} -> goal lv adv) <~ state
    ]

main = layers <~ combine
           [ scene <~ Window.dimensions ~ state
           , stats ]

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
