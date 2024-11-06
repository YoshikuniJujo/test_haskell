{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.ToolsYj (
	list, list', replicateWithI, replicateMWithI,
	elemAll, elemNotAll, findDefault, listToTuple4
	) where

import Data.Maybe
import Data.List ((\\), find)
import Data.List.NonEmpty (NonEmpty(..))

list :: b -> (a -> [a] -> b) -> [a] -> b
list n c = \case [] -> n; x : xs -> c x xs

list' :: b -> (NonEmpty a -> b) -> [a] -> b
list' n c = \case [] -> n; x : xs -> c $ x :| xs

replicateWithI :: Int -> (Int -> a) -> [a]
replicateWithI n f = go 0
	where go i | i < n = f i : go (i + 1) | otherwise = []

replicateMWithI :: Applicative m => Int -> (Int -> m a) -> m [a]
replicateMWithI n f = go 0
	where go i | i < n = (:) <$> f i <*> go (i + 1) | otherwise = pure []

elemAll :: Eq a => [a] -> [a] -> Bool
elemAll es = null . (es \\)

elemNotAll :: Eq a => [a] -> [a] -> Bool
elemNotAll es = not . elemAll es

findDefault :: a -> (a -> Bool) -> [a] -> a
findDefault d p = fromMaybe d . find p

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [r, g, b, a] = (r, g, b, a)
listToTuple4 _ = error "The length of the list is not 4"
