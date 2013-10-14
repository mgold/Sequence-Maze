import Window
import Http (sendGet, Success, Response)
import Dict
import Keyboard (arrows)

data Move = Up | Down | Left | Right
type Frac = Float -- semantically between 0 and 1
type Movement = Maybe (Move, Frac)

moves : Signal (Maybe Move)
moves = let toMove : { x:Int, y:Int } -> Maybe Move
            toMove {x,y} = case (x,y) of
                    (0, 0) -> Nothing
                    (x, 0) -> if x == 1 then Just Right else Just Left
                    (0, y) -> if y == 1 then Just Up else Just Down
                    _ -> Nothing
      in lift toMove arrows |> keepIf isJust Nothing


movement : Signal Movement
movement = let toMovement m = case m of
                    Just d -> Just (d, 1)
                    Nothing -> Nothing
           in toMovement <~ moves


type Player = {i : Int, j : Int,
               dx : Float, dy : Float,
               form : Form}


playerBase = Player 0 0 0 0 (square 30 |> filled black |> rotate (degrees 45))

type Level = {number: Int,
              s: Int,
              w: Int,
              h: Int,
              obstacles: Dict.Dict (Int,Int) Bool
              }

level1 = Level 1 100 4 8 Dict.empty
level : Signal Level
level = constant level1

data Update = UMove Move | ULevel Level

uMoves = moves |> keepIf isJust (Just Right) |> lift (\(Just m) -> UMove m)
uLevel = (\l -> ULevel l) <~ level
update = merge uLevel uMoves

type State = (Level, Player)
state0 : State
state0 = (level1, playerBase)

stepFun : Update -> State -> State
stepFun u s = case u of
    UMove m -> if okMove m s then doMove m s else s
    ULevel l -> (l, playerBase)

state = foldp stepFun state0 update


okMove m (lvl, plyr) = case m of
    Right -> lvl.w > plyr.i +1
    Left -> plyr.i > 0
    Up -> lvl.h > plyr.j +1
    Down -> plyr.j > 0

-- Elm does not support nested record updates
doMove : Move -> State -> State
doMove m (lvl, p) = let
    s = toFloat lvl.s
    frm0 = p.form
    frm = case m of
        Right -> {frm0|x <- frm0.x + s}
        Left  -> {frm0|x <- frm0.x - s}
        Up    -> {frm0|y <- frm0.y + s}
        Down  -> {frm0|y <- frm0.y - s}
    plyr = case m of
        Right -> {p|i <- p.i + 1}
        Left  -> {p|i <- p.i - 1}
        Up    -> {p|j <- p.j + 1}
        Down  -> {p|j <- p.j - 1}
            in (lvl, {plyr|form <- frm})


stats : Signal Element
stats = flow <~ constant down ~ combine
    [ (\lv -> (text . monospace . toText) (show lv.w++"x"++show lv.h++" @"++show lv.s))
        <~ level
    , asText <~ arrows
    , asText <~ moves
    , (\p -> (text . monospace . toText) ("("++show p.i++","++show p.j++")"))
        <~ lift snd state
    ]

-- because Elm doesn't have list comprehensions
pairs xs ys = let n = length ys in case xs of
    [] -> []
    (x::tl) -> (zip (map (\_->x) [1..n]) ys) ++ pairs tl ys

scene : (Int,Int) -> State -> Element
scene (w,h) (lv,p) = let
    s = toFloat lv.s
    center = move (-s * toFloat (lv.w-1) * 0.5, -s * toFloat (lv.h-1) * 0.5)
        in collage w h
        [ rect (toFloat w) (toFloat h) |> filled lightCharcoal
        , map (\(i,j) -> (square (0.99*s) |> filled white
                                          |> move (s*toFloat i, s*toFloat j)))
                (pairs [0..lv.w-1] [0..lv.h-1])
            |> group |> center
        , p.form |> center
        ]

main = layers <~ combine
           [ scene <~ Window.dimensions ~ state
           , stats ]

-- Below: should be library functions

movePolar : (Float, Float) -> Form -> Form
movePolar (r, theta) = move <| fromPolar (r, theta)
