import Window
import Http (sendGet, Success, Response)
import Dict
import Keyboard (arrows)

data Move = Up | Down | Left | Right
type Frac = Float -- semantically between 0 and 1
type Movement = Maybe (Move, Frac)

toMove : { x:Int, y:Int } -> Maybe Move
toMove {x,y} = case (x,y) of
                    (0, 0) -> Nothing
                    (x, 0) -> if x == 1 then Just Right else Just Left
                    (0, y) -> if y == 1 then Just Up else Just Down
                    _ -> Nothing

movement : Signal Movement
movement = let toMovement m = case m of
                    Just d -> Just (d, 1)
                    Nothing -> Nothing
           in toMovement <~ lift toMove arrows


type Player = {i : Int, j : Int,
               dx : Float, dy : Float,
               form : Form}


playerBase = Player 0 0 0 0 (square 30 |> filled black |> rotate (degrees 45))

player : Signal Player
player = constant playerBase

framerate = 50

{-
data Update = Tick dt | Move Move
update : Signal Update
update = let
    clock = (\dt -> Tick dt) <~ fps framerate
    program = parse <~ resp |> dropIf isEmpty [] |> lift (\ms -> Program ms)
        in merge program clock
        -}

type Level = {number: Int,
              s: Int,
              w: Int,
              h: Int,
              obstacles: Dict.Dict (Int,Int) Bool
              }
level : Signal Level
level = constant <| Level 1 100 4 8  Dict.empty

stats : Signal Element
stats = flow <~ constant down ~ combine
    [ (\lv -> (text . monospace . toText) (show lv.w++"x"++show lv.h++" @"++show lv.s))
        <~ level
    , asText <~ arrows
    , asText <~ movement
    ]

-- because Elm doesn't have list comprehensions
pairs xs ys = let n = length ys in case xs of
    [] -> []
    (x::tl) -> (zip (map (\_->x) [1..n]) ys) ++ pairs tl ys

scene : (Int,Int) -> Level -> Player -> Element
scene (w,h) lv p = let s = toFloat lv.s
    in collage w h
    [ rect (toFloat w) (toFloat h) |> filled lightCharcoal
    , map (\(i,j) -> (square (0.99*s) |> filled white
                                      |> move (s*toFloat i, s*toFloat j)))
            (pairs [0..lv.w] [0..lv.h])
        |> group |> move (-s * toFloat lv.w * 0.5, -s * toFloat lv.h * 0.5)
    , p.form
    ]

main = layers <~ combine
           [ scene <~ Window.dimensions ~ level ~ player
           , stats ]

-- Below: should be library functions

movePolar : (Float, Float) -> Form -> Form
movePolar (r, theta) = move <| fromPolar (r, theta)
