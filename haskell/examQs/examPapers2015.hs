import Data.List

revCount :: [a] -> [Int] -> [a]

revCount [x] [0] = [] 
revCount [] [] = []
revCount (x:[]) (c:[]) = [x] ++ revCount [x] [c-1]
revCount (x:xs) (c:cs) = revCount xs cs ++ revCount [x] [c]


afterFilter :: (a -> Bool) -> [a] -> [a]

afterFilter p [] = [] 
afterFilter p [_] = []
afterFilter p (x:xs) = if p x then [head xs] ++ afterFilter p xs else afterFilter p xs