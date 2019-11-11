{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Reimplementation.RealTimeQueue (
	RealTimeQueue, empty, snoc, uncons, isEmpty, head, tail, showRTQ, rotate, foo, ones, showRTQTipes ) where

import Prelude hiding (head, tail)

import Reimplementation.Queue (Queue(..), isEmpty, head, tail)
import Reimplementation.NeverOccur (neverOccur)

import Tools.ShowLazyList

data RealTimeQueue a = RealTimeQueue [a] ![a] [a] deriving Show

instance Queue RealTimeQueue where
	empty = RealTimeQueue [] [] []
	snoc (RealTimeQueue f r []) x =
		let f' = rotate f (x : r) [] in RealTimeQueue f' [] f'
	snoc (RealTimeQueue f r (_ : s)) x = RealTimeQueue f (x : r) s
	uncons (RealTimeQueue [] [] []) = Nothing
	uncons (RealTimeQueue (x : f) r []) =
		let f' = rotate f r [] in Just (x, RealTimeQueue f' [] f')
	uncons (RealTimeQueue (x : f) r (_ : s)) = Just (x, RealTimeQueue f r s)
	uncons (RealTimeQueue [] [] (_ : _)) = neverOccur
	uncons (RealTimeQueue [] (_ : _) _) = neverOccur

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] ys a = reverse ys ++ a
rotate xs [] a = xs ++ a
rotate (x : xs) (y : ys) a = x : rotate xs ys (y : a)
-- rotate (x : xs) (y : ys) a = let xs' =  rotate xs ys (y : a) in x : xs'

showRTQ :: Show a => RealTimeQueue a -> String
showRTQ (RealTimeQueue f r s) = "RealTimeQueue (" ++ showLazyList f ++ ") (" ++ showLazyList r ++ ") (" ++ showLazyList s ++ ")"

showRTQTipes :: Show a => Int -> RealTimeQueue a -> IO ()
showRTQTipes n (RealTimeQueue f r s) = do
	showTipes n f
	showTipes n r
	showTipes n s

foo = rotate "hello" "world" ""
