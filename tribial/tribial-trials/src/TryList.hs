{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryList where

import Data.List

infixIndex :: Eq a => [a] -> [a] -> Maybe Int
infixIndex _ [] = Nothing
infixIndex inf xa@(_ : xs)
	| inf `isPrefixOf` xa = Just 0
	| otherwise = (+ 1) <$> infixIndex inf xs
