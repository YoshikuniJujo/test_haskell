import System.Random
import Data.Time

import qualified SetL as L
import qualified SetT as T

main :: IO ()
main = do
	time (nthL L.empty (randoms $ mkStdGen 8) 10000 :: Maybe Int)
	time (nthL L.empty (randoms $ mkStdGen 8) 20000 :: Maybe Int)
	time (nthL L.empty (randoms $ mkStdGen 8) 40000 :: Maybe Int)
	time (nthL L.empty (randoms $ mkStdGen 8) 80000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 10000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 20000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 40000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 80000 :: Maybe Int)

time :: Show a => a -> IO ()
time x = do
	t0 <- getCurrentTime
	print x
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0

nthL :: Eq a => L.Set a -> [a] -> Int -> Maybe a
nthL s (x : xs) n
	| x `L.member` s = nthL s xs n
	| n < 1 = Just x
	| otherwise = nthL (L.insert x s) xs (n - 1)
nthL _ _ _ = Nothing

nthT :: Ord a => T.Set a -> [a] -> Int -> Maybe a
nthT s (x : xs) n
	| x `T.member` s = nthT s xs n
	| n < 1 = Just x
	| otherwise = nthT (T.insert x s) xs (n - 1)
nthT _ _ _ = Nothing
