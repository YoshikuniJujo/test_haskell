import Data.Time
import System.Random

import qualified Data.Set as S
import qualified SetT as T
import qualified SetB as B

main :: IO ()
main = do
	time (nthS S.empty (randoms $ mkStdGen 8) 20000 :: Maybe Int)
	time (nthS S.empty (randoms $ mkStdGen 8) 40000 :: Maybe Int)
	time (nthS S.empty (randoms $ mkStdGen 8) 80000 :: Maybe Int)
	time (nthS S.empty (randoms $ mkStdGen 8) 160000 :: Maybe Int)
	time (nthS S.empty (randoms $ mkStdGen 8) 320000 :: Maybe Int)
	time (nthS S.empty (randoms $ mkStdGen 8) 640000 :: Maybe Int)
	putStrLn ""
	time (nthT T.empty (randoms $ mkStdGen 8) 20000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 40000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 80000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 160000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 320000 :: Maybe Int)
	time (nthT T.empty (randoms $ mkStdGen 8) 640000 :: Maybe Int)
	putStrLn ""
	time (nthB B.empty (randoms $ mkStdGen 8) 20000 :: Maybe Int)
	time (nthB B.empty (randoms $ mkStdGen 8) 40000 :: Maybe Int)
	time (nthB B.empty (randoms $ mkStdGen 8) 80000 :: Maybe Int)
	time (nthB B.empty (randoms $ mkStdGen 8) 160000 :: Maybe Int)
	time (nthB B.empty (randoms $ mkStdGen 8) 320000 :: Maybe Int)
	time (nthB B.empty (randoms $ mkStdGen 8) 640000 :: Maybe Int)

time :: Show a => a -> IO ()
time x = do
	t0 <- getCurrentTime
	print x
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0

nthS :: Ord a => S.Set a -> [a] -> Int -> Maybe a
nthS s (x : xs) n
	| x `S.member` s = nthS s xs n
	| n < 1 = Just x
	| otherwise = nthS (S.insert x s) xs (n - 1)
nthS _ _ _ = Nothing

nthT :: Ord a => T.Set a -> [a] -> Int -> Maybe a
nthT s (x : xs) n
	| x `T.member` s = nthT s xs n
	| n < 1 = Just x
	| otherwise = nthT (T.insert x s) xs (n - 1)
nthT _ _ _ = Nothing

nthB :: Ord a => B.Set a -> [a] -> Int -> Maybe a
nthB s (x : xs) n
	| x `B.member` s = nthB s xs n
	| n < 1 = Just x
	| otherwise = nthB (B.insert x s) xs (n - 1)
nthB _ _ _ = Nothing
