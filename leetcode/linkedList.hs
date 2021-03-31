
data LinkedList a = EmptyLL
                  | LL { getValue :: a
                       , getNext  :: LinkedList
                       } deriving (Show)

instance Trasversable LinkedList where
    traverse f EmptyLL = EmptyLL
    traverse f LL {v, n} = LL (f v) (traverse f n)
