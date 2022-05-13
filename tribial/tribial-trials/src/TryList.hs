{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryList where

import Prelude hiding (unzip)

import Control.DeepSeq
import Data.Bool
import Data.Maybe
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

unzip'' :: (NFData a, NFData b) => [(a, b)] -> ([a], [b])
unzip'' [] = ([], [])
unzip'' ((x, y) : xys) =
	(x : fst (force $ unzip'' xys), y : snd (force $ unzip'' xys))

modifyElem :: Int -> (a -> a) -> [a] -> [a]
modifyElem ii f xs = (\(i, x) -> bool x (f x) $ i == ii) <$> zip [0 ..] xs

modifyElem' :: Traversable t => Int -> (a -> a) -> t a -> t a
modifyElem' ii f xs = (\(i, x) -> bool x (f x) $ i == ii) <$> fromJust (tzip [0 ..] xs)

tzipWith :: Traversable t => (a -> b -> c) -> [a] -> t b -> Maybe (t c)
tzipWith f xs = sequenceA . snd . mapAccumL pair xs
	where
	pair [] _ = ([], Nothing)
	pair (x : xs') y = (xs', Just (f x y))

tzip :: Traversable t => [a] -> t b -> Maybe (t (a, b))
tzip = tzipWith (,)
