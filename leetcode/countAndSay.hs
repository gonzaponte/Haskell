import Data.List

say :: String -> String
say "" = ""
say ss = let h        = head ss
             (hs, ts) = span (==h) ss
             n        = length hs
         in  show n ++ h : say ts


countAndSay :: Int -> String
countAndSay 1 = "1"
countAndSay n = say . countAndSay . pred $ n
