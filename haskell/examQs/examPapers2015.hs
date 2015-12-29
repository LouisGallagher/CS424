import Data.List

--revCount :: [a] -> [Int] -> [a]

revCount [x] [0] = [] 
revCount [] [] = []
revCount (x:[]) (c:[]) = [x] ++ revCount [x] [c-1]
revCount (x:xs) (c:cs) = revCount xs cs ++ revCount [x] [c]