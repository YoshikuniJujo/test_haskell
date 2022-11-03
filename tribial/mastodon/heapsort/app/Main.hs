{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Bool

main :: IO ()
main = print $ hsort [4, 3, 5, 7, 1, 8, 6, 2]

hsort :: Ord a => [a] -> [a]
hsort xs = runST do
	a <- toArray xs
	ln <- rangeSize <$> getBounds a
	putHere a ln `mapM_` [ln - 1, ln - 2 .. 0]
	putTail a `mapM_` [ln, ln - 1 .. 1]
	getElems a

toArray :: [a] -> ST s (STArray s Int a)
toArray xs = newListArray (0, length xs - 1) xs

putTail :: Ord a => STArray s Int a -> Int -> ST s ()
putTail a sz = do
	b <- readArray a 0
	x <- readArray a (sz - 1)
	writeArray a (sz - 1) b
	put a (sz - 1) 0 x

putHere :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
putHere a sz i = do
	x <- readArray a i
	put a sz i x

put :: Ord a => STArray s Int a -> Int -> Int -> a -> ST s ()
put a sz i x = do
	mt <- target a sz i x
	case mt of
		Nothing -> writeArray a i x
		Just t -> do
			x' <- readArray a t
			writeArray a i x'
			put a sz t x

lft, rgt :: Int -> Int
lft = (+ 1) . (* 2)
rgt = (+ 2) . (* 2)

target :: Ord a => STArray s Int a -> Int -> Int -> a -> ST s (Maybe Int)
target a sz i x
	| li >= sz = pure Nothing
	| ri >= sz = do
		l <- readArray a li
		pure $ bool (Nothing) (Just li) (l > x)
	| otherwise = do
		l <- readArray a li
		r <- readArray a ri
		pure if l > r
			then bool (Nothing) (Just li) (l > x)
			else bool (Nothing) (Just ri) (r > x)
	where li = lft i; ri = rgt i
