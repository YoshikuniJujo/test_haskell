{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BitonicSort where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Bool
import Data.Array.MArray
import Data.Array.ST
import System.Random

bitonicSortPairs :: Integral i => Bool -> i -> i -> [[(i, i)]]
bitonicSortPairs _ _ 0 = []
bitonicSortPairs fl i n =
	zipWith (++)
		(bitonicSortPairs fl i (n - 1))
		(bitonicSortPairs (not fl) (i + 2 ^ (n - 1)) (n - 1)) ++
	bitonicMergePairs fl i n

bitonicMergePairs :: Integral i => Bool -> i -> i -> [[(i, i)]]
bitonicMergePairs _ _ 0 = []
bitonicMergePairs fl i n =
	(bool id flip fl zip)
		[i, i + 1 .. i + 2 ^ (n - 1) - 1]
		[i + 2 ^ (n - 1) , i + 2 ^ (n - 1) + 1 .. i + 2 ^ n - 1] :
	zipWith (++)
		(bitonicMergePairs fl i (n - 1))
		(bitonicMergePairs fl (i + 2 ^ (n - 1)) (n - 1))

swapIfNeed :: (MArray a e m, Ix i, Ord e) => i -> i -> a i e -> m ()
swapIfNeed imn imx a = do
	mn <- readArray a imn
	mx <- readArray a imx
	when (mn > mx) do
		writeArray a imn mx
		writeArray a imx mn

runSort :: (MArray a e m, Ix i, Ord e) => [[(i, i)]] -> a i e -> m ()
runSort ipss a = for_ ipss \ips -> for_ ips \ip -> uncurry swapIfNeed ip a

getIndex :: Integral i => i -> i -> i
getIndex i0 n
	| 2 ^ i0 >= n = i0
	| otherwise = getIndex (i0 + 1) n

sort :: forall e . (Ord e, Bounded e) => [e] -> [e]
sort xs = take ln $ runST do
	(a :: STArray s Int e) <- newListArray (0, 2 ^ i - 1) (xs ++ replicate (2 ^ i - ln) maxBound) ::
		ST s (STArray s Int e)
	runSort (bitonicSortPairs False 0 i) a
	getElems a
	where
	ln = length xs
	i = getIndex 0 ln

makeSample :: Int -> IO [Int]
makeSample n = take n . randoms <$> getStdGen

checkSort :: Ord x => [x] -> Bool
checkSort xs = all (uncurry (<=)) $ zip xs (tail xs)
