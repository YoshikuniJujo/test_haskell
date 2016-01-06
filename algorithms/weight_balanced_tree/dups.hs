import Data.Maybe
import Data.Time
import System.Random

import qualified MapL as L
import qualified MapT as T

main :: IO ()
main = do
	time . dupsL L.empty 0 $ take 10000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsL L.empty 0 $ take 20000 (randoms $ mkStdGen 8 :: [Int])
--	time . dupsL L.empty 0 $ take 40000 (randoms $ mkStdGen 8 :: [Int])
--	time . dupsL L.empty 0 $ take 80000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 10000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 20000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 40000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 80000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 160000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 320000 (randoms $ mkStdGen 8 :: [Int])

time :: Show a => a -> IO ()
time x = do
	t0 <- getCurrentTime
	print x
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0

dupsL :: Eq a => L.Map a Int -> Int -> [a] -> Int
dupsL d mx (x : xs) = dupsL (L.insert x t d) (max mx t) xs
	where t = fromMaybe 0 (x `L.lookup` d) + 1
dupsL _ mx _ = mx

dupsT :: Ord a => T.Map a Int -> Int -> [a] -> Int
dupsT d mx (x : xs) = dupsT (T.insert x t d) (max mx t) xs
	where t = fromMaybe 0 (x `T.lookup` d) + 1
dupsT _ mx _ = mx
