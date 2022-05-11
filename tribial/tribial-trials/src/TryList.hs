{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryList where

import Prelude hiding (unzip)
import Data.List hiding (unzip)

infixIndex :: Eq a => [a] -> [a] -> Maybe Int
infixIndex _ [] = Nothing
infixIndex inf xa@(_ : xs)
	| inf `isPrefixOf` xa = Just 0
	| otherwise = (+ 1) <$> infixIndex inf xs

infixIndex' :: Eq a => [a] -> [a] -> Maybe Int
infixIndex' _ [] = Nothing
infixIndex' inf@(x : _) ya@(y : ys)
	| x == y = if inf `isPrefixOf` ya then Just 0 else Nothing
	| otherwise = (+ 1) <$> infixIndex' inf ys

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y) : xys) = let (xs, ys) = unzip xys in (x : xs, y : ys)

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y) : xys) = (x : fst (unzip' xys), y : snd (unzip' xys))
