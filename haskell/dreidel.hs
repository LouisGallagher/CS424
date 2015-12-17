import Dist
import Data.List
import Data.Ord


choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k- 1) * n `div` k 

-- computes p(x == s) i.e the probability that the number of successes equals s, given the other parameters
binom :: Int -> Int -> Double -> Double
binom t s ps = fromIntegral (choose t s) * ((ps ** fromIntegral s) * ((1 - ps) ** fromIntegral(t - s)))

profit :: Int->  Double -> Double -> Double
profit wins pot betsize = pot * ((1 + (betsize * 9)) ** fromIntegral wins) 

loss :: Int -> Double -> Double -> Double
loss losses pot betsize = pot * ((1 - betsize) ** fromIntegral losses)


dreidelDreidelDreidel :: Double -> Double -> Int -> Dist Double
dreidelDreidelDreidel y0 p n = Dist[(loss (n-i) (profit i y0 p) p ,binom n i 0.25) | i <- [0..n]]


mean :: Dist Double -> Double 
mean (Dist dist) = sum(map (\x -> fst x * snd x) dist) --(sum (map fst dist)) / fromIntegral(length dist) -- wrong should be sum (value * prob)

maximizeExpRet :: Double -> Int -> Double 
maximizeExpRet y0 n = fst(maximumBy (comparing snd) ([((fromIntegral i * 0.01), mean(dreidelDreidelDreidel y0 (fromIntegral i * 0.1) n)) | i <-[0..100]]))



prExceeds :: Double -> Dist Double -> Double
prExceeds target dist = mean $ fmap (fromIntegral . fromEnum . (>= target)) dist


snd3 :: (a, b, c) -> b 
snd3 (_, b, _) = b 


--returns 3-tuple of the form (p, probability returns exceed target, mean value)
maximizePrExceeds :: Double -> Double -> Int -> (Double, Double, Double)
maximizePrExceeds y0 target n = maximumBy (comparing snd3) ([(fromIntegral i * 0.01, prExceeds target (dreidelDreidelDreidel y0 (fromIntegral i * 0.01) n), mean (dreidelDreidelDreidel y0 (fromIntegral i * 0.01) n)) | i<-[0..100]])


-------------------------------------------------- Sample Runs and answers------------------------------------------------------
{-
-- find p that maximizes expected return
> maximizeExpRet 1000 10
1.0

-- find p that maximizes probabilty that return exceeds 4000 after 10 rounds of play
>maximizePrExceeds 1000 4000 10
(0.31,0.4744071960449219,45538.24055304078) 

-}
