import Window
import Http (sendGet, Success, Response)
import Set
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

goalForm : Form
goalForm = square 50 |> filled yellow

arrowForm : Move -> Form
arrowForm m = case m of
    Right -> ngon 3 30 |> filled green
    Up ->    ngon 3 30 |> filled blue   |> rotate (turns 0.25)
    Left ->  ngon 3 30 |> filled purple |> rotate (turns 0.5)
    Down ->  ngon 3 30 |> filled red    |> rotate (turns 0.75)

type Level = {number: Int,
              s: Int,
              w: Int,
              h: Int,
              start: Coord,
              goal : Coord,
              obstacles: Set.Set Coord,
              seq: [(Coord, Move)]
              }

level1 = Level 1 100 5 4 (0,0) (4,3)
            (Set.fromList [(1,0),(1,1),(1,2), (3,3),(3,2),(3,1)])

 [((0,1), Up), ((0,2), Up), ((0,3), Right), ((1,3), Right), ((2,3), Down)]

level : Signal Level
level = constant level1

data Update = UMove Move | ULevel Level

uMoves = moves |> keepIf isJust (Just Right) |> lift (\(Just m) -> UMove m)
uLevel = ULevel <~ level
update = merge uLevel uMoves

type State = (Level, Player)
state0 : State
state0 = (level1, playerBase level1.start)

stepFun : Update -> State -> State
stepFun u s = case u of
    UMove m -> if okMove m s then doMove m s else s
    ULevel l -> (l, playerBase level1.start)

state = foldp stepFun state0 update

okMove m (lvl, plyr) = case m of
    Right -> lvl.w > plyr.i +1
                && not (Set.member (plyr.i+1, plyr.j) lvl.obstacles)
    Left -> plyr.i > 0
                && not (Set.member (plyr.i-1, plyr.j) lvl.obstacles)
    Up -> lvl.h > plyr.j +1
                && not (Set.member (plyr.i, plyr.j+1) lvl.obstacles)
    Down -> plyr.j > 0
                && not (Set.member (plyr.i, plyr.j-1) lvl.obstacles)

doMove : Move -> State -> State
doMove m (lv, p) = (lv, case m of
    Right -> {p|i <- p.i + 1}
    Left  -> {p|i <- p.i - 1}
    Up    -> {p|j <- p.j + 1}
    Down  -> {p|j <- p.j - 1}
        )

drawPlayer : State -> Form
drawPlayer (lv, p) = playerForm |> move (toFloat (lv.s*p.i), toFloat (lv.s*p.j))

drawGoal : Level -> Form
drawGoal lv = move (both (\i -> toFloat <| lv.s*i) lv.goal) goalForm

drawArrow : Int -> (Coord, Move) -> Form
drawArrow s (c, m) = move (both (\i -> toFloat <| s*i) c) (arrowForm m)

stats : Signal Element
stats = flow <~ constant down ~ combine
    [ (\lv -> (text . monospace . toText) (show lv.w++"x"++show lv.h++" @"++show lv.s))
        <~ level
    , asText <~ arrows
    , asText <~ moves
    , (\p -> (text . monospace . toText) ("("++show p.i++","++show p.j++")"))
        <~ lift snd state
    ]

scene : (Int,Int) -> State -> Element
scene (w,h) (lv,p) = let
    s = toFloat lv.s
    center = move (-s * toFloat (lv.w-1) * 0.5, -s * toFloat (lv.h-1) * 0.5)
        in collage w h (
        [ rect (toFloat w) (toFloat h) |> filled lightCharcoal
        , map (\(i,j) -> (square (0.99*s) |> filled white
                                          |> move (s*toFloat i, s*toFloat j)))
                (pairs [0..lv.w-1] [0..lv.h-1])
            |> group |> center
        , map (\(i,j) -> (square (0.9*s) |> filled darkCharcoal
                                         |> move (s*toFloat i, s*toFloat j)))
                (Set.toList lv.obstacles)
            |> group |> center
        ] ++ (map (center . drawArrow lv.s) lv.seq) ++
        [ drawGoal lv |> center
        , drawPlayer (lv,p) |> center
        ]
        )

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

