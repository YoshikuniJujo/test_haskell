{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Given where

import Data.Maybe

import Expression

newtype Given i v = Given [Expression i v] deriving Show

removeVar :: (Integral i, Ord v) => Given i v -> v -> Given i v
removeVar (Given es) = Given . remVar es

remVar :: (Integral i, Ord v) => [Expression i v] -> v -> [Expression i v]
remVar [] _ = []
remVar (e : es) v 
	| includeVar e v = remVarOf e v es
	| otherwise = e : remVar es v

remVarOf :: (Integral i, Ord v) => Expression i v -> v -> [Expression i v] -> [Expression i v] 
remVarOf e0 v es = replace (\e -> annihilation e e0 v) es

replace :: (a -> Maybe a) -> [a] -> [a]
replace _ [] = []
replace f (x : xs) = fromMaybe x (f x) : replace f xs

example :: Given Integer Char
example = Given [
	var 'm' .- num 1 .- var 'x',
	var 'm' .- num 3 .- var 'y',
	var 'x' .- num 1 .- var 'z',
	var 'z' .- num 1 .- var 'w',
	var 'w' .- num 1 .- var 'v' ]
