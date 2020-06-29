rlength x = if null x
              then 0
              else 1 + rlength(tail x)

plength :: (Num b) => [a] -> b
plength [] = 0
plength (x:xs) = 1 + plength xs

rsum x = if null x
           then 0
           else head x + rsum(tail x)

psum :: (Num a) => [a] -> a
psum [] = 0
psum (x:xs) = x + psum xs

cumsum [] = []
cumsum x  = (sum x): cumsum (tail x)

-- ##############################################################

lastButOne x = head (drop ((length x) - 2) x)

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455
              "Algebra of Programming"
              ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)


bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

-- data Maybe a = Just a
--              | Nothing


data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList (x:xs) = Cons x (toList xs)
toList []     = Nil

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- Write the converse of fromList for the List type: a function that
-- takes a List a and generates a [a].
fromList Nil = []
fromList (Cons x b) = x : (fromList b)

-- Define a tree type that has only one constructor, like our Java
-- example. Instead of the Empty constructor, use the Maybe type to
-- refer to a node's children.
data MyTree a = MyNode a (Maybe (MyTree a)) (Maybe (MyTree a))
                deriving (Show)

