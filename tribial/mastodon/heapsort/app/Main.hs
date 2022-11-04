{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Bool
import System.Random (randomRIO, randomRs, getStdGen)

main :: IO ()
main = do
	n <- randomRIO (0, 30)
	ns <- randomRs @Int (1, 100) <$> getStdGen
	let	srt = hsortList $ take n ns
	print srt
	print $ checkSort srt

checkSort :: Ord a => [a] -> Bool
checkSort = \case
	[] -> True
	[_] -> True
	x : xs@(y : _)
		| x <= y -> checkSort xs
		| otherwise -> False

hsortList :: Ord a => [a] -> [a]
hsortList xs = runST
	$ (>>) <$> hsort <*> getElems =<< newListArray (0, length xs - 1) xs

hsort :: Ord a => STArray s Int a -> ST s ()
hsort a = rangeSize <$> getBounds a >>= \s ->
	(>>) <$> (treeize a s `mapM_`) <*> (linear a `mapM_`)
		$ [s - 1, s - 2 .. 0]

treeize :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
treeize a s i = put a s i =<< readArray a i

linear :: Ord a => STArray s Int a -> Int -> ST s ()
linear a s = do
	[b, x] <- readArray a `mapM` [0, s]
	writeArray a s b
	put a s 0 x

put :: Ord a => STArray s Int a -> Int -> Int -> a -> ST s ()
put a s i x = next a s i x >>= \case
	Nothing -> writeArray a i x
	Just t -> (writeArray a i =<< readArray a t) >> put a s t x

next :: Ord a => STArray s Int a -> Int -> Int -> a -> ST s (Maybe Int)
next a s i x
	| h >= s = pure Nothing
	| j >= s = rt h <$> readArray a h
	| True = do
		[l, r] <- readArray a `mapM` [h, j]
		pure $ bool (rt h l) (rt j r) (l <= r)
	where
	h = i * 2 + 1; j = h + 1
	rt k y = bool Nothing (Just k) (y > x)
