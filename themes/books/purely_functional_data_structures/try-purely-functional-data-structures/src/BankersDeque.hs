{-# OPTIONS_GHC -Wall -fno-warn-tabs #-} 

module BankersDeque where

import Prelude hiding (head, tail, init, last)

import Control.Exception

import Queue

data DQ a = DQ Int Int [a] Int [a] deriving Show

instance Queue DQ where
	empty = DQ 2 0 [] 0 []
	snoc (DQ c lenf f lenr r) x = check $ DQ c lenf f (lenr + 1) (x : r)
	uncons (DQ _ 0 [] 0 []) = Nothing
	uncons (DQ _ 0 [] 1 [x]) = Just (x, empty)
	uncons (DQ c lenf (x : f) lenr r) = Just (x, check $ DQ c (lenf - 1) f lenr r)
	uncons (DQ _ lenf f lenr r) = throw . FormatError $
		show lenf ++ " " ++ show (length f) ++ " " ++ show lenr ++ " " ++ show (length r)

instance Deque DQ where
	rev (DQ c lenf f lenr r) = DQ c lenr r lenf f

check :: DQ a -> DQ a
check dq@(DQ c lenf f lenr r)
	| lenf > c * lenr + 1 = let
		i = (lenf + lenr) `div` 2
		j = lenf + lenr - i
		f' = take i f
		r' = r ++ reverse (drop i f) in DQ c i f' j r'
	| lenr > c * lenf + 1 = let
		j = (lenf + lenr) `div` 2
		i = lenf + lenr - j
		r' = take j r
		f' = f ++ reverse (drop j r) in DQ c i f' j r'
	| otherwise = dq

data FormatError = FormatError String deriving Show
instance Exception FormatError
