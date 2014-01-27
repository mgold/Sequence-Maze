module Arrow where

data Move = Up | Down | Left | Right

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
