module Level where

import Arrow (Move, Left, Right, Up, Down)

type Coord = (Int, Int)
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

-- Below: should be library functions

succ = (+) 1

nth : Int -> [a] -> a
nth i xs = head <| drop i xs
