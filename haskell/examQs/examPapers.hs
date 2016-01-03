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


merge:: (a->a->Bool) -> [a] -> [a] -> [a]
merge p [] [] = []
merge p [x] [y] = if p x y then [x, y] else [y, x]
merge p (x: xs) (y:ys) = merge p [x] [y] ++ merge p xs ys 

mySort :: (a->a->Bool) -> [a] -> [a]
mySort p [] = []
mySort p [x] = [x]
mySort p xs = merge p (mySort p (take half xs)) (mySort p (drop half xs)) 
			where
			half = (length xs) `div` 2  

mapEveryOther:: (a -> a) -> [a] -> [a]
mapEveryOther f [] = []
mapEveryOther f [x] = [f x]
mapEveryOther f (x:y:xs) = [f x, y] ++ mapEveryOther f xs