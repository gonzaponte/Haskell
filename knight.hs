
type KnightPos = (Int, Int)

onBoard :: KnightPos -> Bool
onBoard (c, r) = elem c [1..8] && elem r [1..8]


move :: KnightPos -> [KnightPos]
move (c, r) = let allPos = [ (c-2, r-1), (c-2, r+1), (c-1, r-2), (c-1, r+2)
                           , (c+2, r-1), (c+2, r+1), (c+1, r-2), (c+1, r+2)
                           ]
              in filter onBoard allPos

shortestRoute :: KnightPos -> KnightPos -> [[KnightPos]]
shortestRoute a b = let in1 = 
