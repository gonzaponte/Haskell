import qualified Data.Foldable as F


data Tree a = Empty
            | Node a (Tree a) (Tree a)
--            deriving (Show, Read, Eq)


instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                                     f x `mappend`
                           F.foldMap f r


demoTree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

main = print $ foldr (+) 0 demoTree
