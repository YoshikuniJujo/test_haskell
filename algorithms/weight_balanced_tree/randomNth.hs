import System.Random
import Data.Time

import qualified SetL as L
import qualified SetT as T

main :: IO ()
main = do
	time (nthL L.empty (mkStdGen 8) 40000 :: Int)
	time (nthS T.empty (mkStdGen 8) 40000 :: Int)
	time (nthLL L.empty (randoms $ mkStdGen 8) 40000 :: Maybe Int)
	time (nthLS T.empty (randoms $ mkStdGen 8) 40000 :: Maybe Int)

time :: Show a => a -> IO ()
time x = do
	t0 <- getCurrentTime
	print x
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0

nth0 :: Random a => StdGen -> Int -> a
nth0 g n
	| n < 1 = x
	| otherwise = nth0 g' (n - 1)
	where
	(x, g') = random g

nthL :: (Eq a, Random a) => L.Set a -> StdGen -> Int -> a
nthL s g n
	| x `L.member` s = nthL s g' n
	| n < 1 = x
	| otherwise = nthL (L.insert x s) g' (n - 1)
	where
	(x, g') = random g

nthS :: (Ord a, Random a) => T.Set a -> StdGen -> Int -> a
nthS s g n
	| x `T.member` s = nthS s g' n
	| n < 1 = x
	| otherwise = nthS (T.insert x s) g' (n - 1)
	where
	(x, g') = random g
nthLL :: Eq a => L.Set a -> [a] -> Int -> Maybe a
nthLL s (x : xs) n
	| x `L.member` s = nthLL s xs n
	| n < 1 = Just x
	| otherwise = nthLL (L.insert x s) xs (n - 1)
nthLL _ _ _ = Nothing

nthLS :: Ord a => T.Set a -> [a] -> Int -> Maybe a
nthLS s (x : xs) n
	| x `T.member` s = nthLS s xs n
	| n < 1 = Just x
	| otherwise = nthLS (T.insert x s) xs (n - 1)
nthLS _ _ _ = Nothing
