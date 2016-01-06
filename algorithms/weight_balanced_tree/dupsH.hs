import Control.Applicative
import Data.Maybe
import Data.Time
import System.Random

import qualified MapL as L
-- import qualified MapT as T
import qualified Data.Map as T
import qualified Data.HashTable.IO as H
import qualified Data.Hashable as H

main :: IO ()
main = do
	time . dupsL L.empty 0 $ take 10000 (randoms $ mkStdGen 8 :: [Int])
--	time . dupsL L.empty 0 $ take 20000 (randoms $ mkStdGen 8 :: [Int])
--	time . dupsL L.empty 0 $ take 40000 (randoms $ mkStdGen 8 :: [Int])
--	time . dupsL L.empty 0 $ take 80000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 10000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 20000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 40000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 80000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 160000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 320000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 640000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 1280000 (randoms $ mkStdGen 8 :: [Int])
	time . dupsT T.empty 0 $ take 2560000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 10000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 20000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 40000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 80000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 160000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 320000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 640000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 1280000 (randoms $ mkStdGen 8 :: [Int])
	timeH $ take 2560000 (randoms $ mkStdGen 8 :: [Int])

time :: Show a => a -> IO ()
time x = do
	t0 <- getCurrentTime
	print x
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0

dupsL :: Eq a => L.Map a Int -> Int -> [a] -> Int
dupsL d mx (x : xs) = let t = fromMaybe 0 (x `L.lookup` d) + 1 in
	dupsL (L.insert x t d) (max mx t) xs
dupsL _ mx _ = mx

dupsT :: Ord a => T.Map a Int -> Int -> [a] -> Int
dupsT d mx (x : xs) = let t = fromMaybe 0 (x `T.lookup` d) + 1 in
	dupsT (T.insert x t d) (max mx t) xs
dupsT _ mx _ = mx

timeH :: [Int] -> IO ()
timeH xs = do
	t0 <- getCurrentTime
	d <- H.new
	dupsH d 0 xs >>= print
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0

dupsH :: (Eq a, H.Hashable a) => H.BasicHashTable a Int -> Int -> [a] -> IO Int
dupsH d mx (x : xs) = do
	t <- (+ 1) . fromMaybe 0 <$> d `H.lookup` x
	H.insert d x t
	dupsH d (max mx t) xs
dupsH _ mx _ = return mx
