-- CIS 194 Homework 2

module Log where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file

toInt :: String -> Int
toInt s = read s::Int

pickMessage :: [String] -> String
pickMessage ss
  | "E" == head ss = unwords $ drop 3 ss
  | otherwise      = unwords $ drop 2 ss


pickTimestamp :: [String] -> Int
pickTimestamp ss
  | "E" == head ss = toInt $ ss !! 2
  | otherwise      = toInt $ ss !! 1


pickError :: [String] -> MessageType
pickError ss
  | letter == "E" = Error $ toInt $ ss !! 1
  | letter == "I" = Info
  | letter == "W" = Warning
  where letter = ss !! 0


validError :: [String] -> Bool
validError ss =  (head ss) `elem` ["E", "W", "I"]

parseMessage :: String -> LogMessage
parseMessage s
  | valid     = LogMessage (pickError tokens) (pickTimestamp tokens) (pickMessage tokens)
  | otherwise = Unknown s
  where tokens = words s
        valid  = validError tokens


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s


timeMessage :: LogMessage -> TimeStamp
timeMessage (Unknown _       ) = 0
timeMessage (LogMessage _ t _) = t


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert new Leaf = Node Leaf new Leaf
insert new (Node left message right)
  | tMess < tNode = Node (insert new left) message right
  | tMess > tNode = Node left message (insert new right)
  | otherwise     = error "Two messages with the same time"
  where tMess = timeMessage new
        tNode = timeMessage message


build :: [LogMessage] -> MessageTree
build []        = Leaf
build (lm:rest) = insert lm (build rest)


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right


isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error level) _ _) = level >= 50
isSevere _ = False


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = let tree    = build messages
                             ordered = inOrder tree
                         in [m | (LogMessage _ _ m) <- filter isSevere ordered]
