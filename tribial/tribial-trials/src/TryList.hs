{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryList where

import Data.List

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
