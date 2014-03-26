module Level where

import Arrow (Move, Left, Right, Up, Down)

type Coord = (Int, Int)
type Sequence = [(Coord, Move)]

type Level = {number: Int,
              side: Int,
              w: Int,
              h: Int,
              start: Coord,
              obsRight: [Coord],
              obsUp: [Coord],
              seq: [Sequence],
              goal : Coord
              }

isSubgoal : Level -> Int -> Bool
isSubgoal lv adv = succ adv < length lv.seq

goal : Level -> Int -> Coord
goal lv adv = if isSubgoal lv adv
              then lv.seq |> unsafeNth (succ adv) |> head |> fst
              else lv.goal

levels : [Level]
levels = [
    Level 0 200 3 2 (0,0) [(0,0)] [(1,0)]
    [ [((0,0), Up), ((0,1), Right)]
    , [((1,1), Right), ((2,1), Down), ((2,0), Left)]
    ] (1,0)
    ,
    Level 1 180 2 4 (0,0) [(0,1), (0,2)] [(1,0)]
    [ [((0,0), Up), ((0,1), Up), ((0,2), Up)]
    , [((0,3), Right), ((1,3), Down), ((1,2), Down)]
    ] (1,1)
    ,
    Level 2 200 3 3 (2,0) [(0,1)] [(1,0), (2,0), (1,1)]
    [ [((2,0), Left), ((1,0), Left), ((0,0), Up), ((0,1), Up), ((0,2), Right)]
    , [((1,2), Right), ((2,2), Down), ((2,1), Left)]
    ] (1,1)
    ,
    Level 3 200 5 3 (0,1) [(0,1)] [(1,0), (2,0), (3,0), (4,0), (1,1), (2,1), (3,1)]
    [ [((0,1), Up), ((0,2), Right), ((1,2), Right), ((2,2), Right), ((3,2), Right), ((4,2), Down)]
    , [((4,1), Left), ((3,1), Left), ((2,1), Left)]
    ] (1,1)
    ,
    Level 4 160 5 4 (0,3) [(3,1)] [(0,0), (1,0), (2,0), (1,1), (2,1), (3,1),
    (4,1), (0,2), (1,2), (2,2), (4,2)]
    [ [((0,3), Right), ((1,3), Right), ((2,3), Right), ((3,3), Down)]
    , [((3,2), Left), ((2,2), Left), ((1,2), Left), ((0,2), Down), ((0,1), Right)]
    , [((1,1), Right), ((2,1), Right), ((3,1), Down), ((3,0), Right), ((4,0), Up)]
    ] (4,1)

    ]

level0 = head levels

-- Below: should be library functions

succ = (+) 1


unsafeNth : Int -> [a] -> a
unsafeNth i xs = head <| drop i xs

nth : Int -> [a] -> Maybe a
nth i xs = case drop i xs of
    [] -> Nothing
    x::_ -> Just x
