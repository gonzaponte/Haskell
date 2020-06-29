import Data.List

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h


makeTree :: [a] -> Tree a
makeTree []  = Leaf
makeTree [x] = Node 1 Leaf x Leaf
makeTree xs  = let this    = last xs
                   rest    = init xs
                   half    = (genericLength rest) `div` 2
                   (toLeft, toRight) = splitAt half rest
                   left    = makeTree toLeft
                   right   = makeTree toRight
                   mheight = succ . maximum $ map height [left, right]
               in Node mheight left this right

treeInsert :: a -> Tree a -> Tree a
treeInsert new Leaf = Node 1 Leaf new Leaf
treeInsert new (Node h left old right)
  | hl <= hr  = let newLeft = treeInsert new left
                in  Node (1 + height newLeft) newLeft old right
  | otherwise = let newRight = treeInsert new right
                in  Node (1 + height newRight) left old newRight
  where hl = height left
        hr = height right

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf