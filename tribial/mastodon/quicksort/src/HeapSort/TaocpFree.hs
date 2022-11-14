{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeapSort.TaocpFree where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool
import System.Random

mkSample :: (Int, Int) -> IO [Int]
mkSample r = do
	g <- newStdGen
	let	(n, g') = randomR (0, 100) g
	pure . take n $ randomRs r g'

mkSample' :: (Int, Int) -> Int -> IO [Int]
mkSample' r n = do
	g <- newStdGen
	pure . take n $ randomRs r g

checkSample :: Ord a => [a] -> Bool
checkSample = \case
	[] -> True
	[_] -> True
	x : xs@(y : _)
		| x <= y -> checkSample xs
		| otherwise -> False

heapsort :: Ord a => [a] -> [a]
heapsort xs = runST
	$ (>>) <$> hsort <*> getElems =<< newListArray (1, length xs) xs

hsort :: Ord a => STArray s Int a -> ST s ()
hsort ks = do
	(1, n) <- getBounds ks
	for_ [n `div` 2, n `div` 2 - 1 .. 1] \l ->
		shiftup ks n l =<< readArray ks l
	for_ [n, n - 1 .. 2] \r -> readArray ks r >>= \k -> do
		writeArray ks r =<< readArray ks 1
		shiftup ks (r - 1) 1 k

shiftup :: Ord a => STArray s Int a -> Int -> Int -> a -> ST s ()
shiftup ks r j k
	| cl <= r = do
		kcl <- readArray ks cl
		(cb, kcb) <- if cl < r
			then do	kcr <- readArray ks cr
				pure $ bool (cl, kcl) (cr, kcr) (kcl < kcr)
			else pure (cl, kcl)
		if k >= kcb
			then writeArray ks j k
			else writeArray ks j kcb >> shiftup ks r cb k
	| otherwise = writeArray ks j k
	where cl = 2 * j; cr = cl + 1
