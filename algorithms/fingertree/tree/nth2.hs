import Data.Time

import qualified SetT as T
import qualified SetB as B

main :: IO ()
main = do
	time (nthT T.empty [0 ..] 20000 :: Maybe Int)
	time (nthT T.empty [0 ..] 40000 :: Maybe Int)
	time (nthT T.empty [0 ..] 80000 :: Maybe Int)
	time (nthT T.empty [0 ..] 160000 :: Maybe Int)
	time (nthT T.empty [0 ..] 320000 :: Maybe Int)
	time (nthT T.empty [0 ..] 640000 :: Maybe Int)
	putStrLn ""
	time (nthB B.empty [0 ..] 20000 :: Maybe Int)
	time (nthB B.empty [0 ..] 40000 :: Maybe Int)
	time (nthB B.empty [0 ..] 80000 :: Maybe Int)
	time (nthB B.empty [0 ..] 160000 :: Maybe Int)
	time (nthB B.empty [0 ..] 320000 :: Maybe Int)
	time (nthB B.empty [0 ..] 640000 :: Maybe Int)

time :: Show a => a -> IO ()
time x = do
	t0 <- getCurrentTime
	print x
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0

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
