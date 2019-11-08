{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RealTimeDeque where

import Prelude hiding (head, tail, init, last)

import Control.Exception

import Queue

data DQ a = DQ Int Int [a] [a] Int [a] [a] deriving Show

rotateRev :: Int -> [a] -> [a] -> [a] -> [a]
rotateRev _ [] r a = reverse r ++ a
rotateRev c (x : f) r a =
	x : rotateRev c f (drop c r) (reverse (take c r) ++ a)

rotateDrop :: Int -> [a] -> Int -> [a] -> [a]
rotateDrop c f j r
	| j < c = rotateRev c f (drop j r) []
	| otherwise = let x : f' = f in x : rotateDrop c f' (j - c) (drop c r)

instance Queue DQ where
	empty = DQ 2 0 [] [] 0 [] []
	snoc (DQ c lenf f sf lenr r sr) x =
		check $ DQ c lenf f (exec1 sf) (lenr + 1) (x : r) (exec1 sr)
	uncons (DQ _ 0 [] _ 0 [] _) = Nothing
	uncons (DQ _ 0 [] _ 1 [x] _) = Just (x, empty)
	uncons (DQ c lenf (x : f') sf lenr r sr) =
		Just (x, check $ DQ c (lenf - 1) f' (exec2 sf) lenr r (exec2 sr))
	uncons _ = throw FormatError

instance Deque DQ where
	rev (DQ c lenf f sf lenr r sr) = DQ c lenr r sr lenf f sf

exec1, exec2 :: [a] -> [a]
exec1 (_ : s) = s; exec1 [] = []
exec2 = exec1 . exec1

check :: DQ a -> DQ a
check q@(DQ c lenf f _ lenr r _)
	| lenf > c * lenr + 1 = let
		i = (lenf + lenr) `div` 2
		j = lenf + lenr - i
		f' = take i f
		r' = rotateDrop c r i f in DQ c i f' f' j r' r'
	| lenr > c * lenf + 1 = let
		j = (lenf + lenr) `div` 2
		i = lenf + lenr - j
		r' = take j r
		f' = rotateDrop c f j r in DQ c i f' f' j r' r'
	| otherwise = q

data FormatError = FormatError deriving Show

instance Exception FormatError
