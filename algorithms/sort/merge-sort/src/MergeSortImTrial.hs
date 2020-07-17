{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSortImTrial where

import Control.Monad.Identity
import Control.Monad.State
import System.Random

state :: (s -> (a, s)) -> State s a
state f = StateT $ Identity . f

sepAscDesc :: Ord a => [a] -> [[a]]
sepAscDesc [] = []
sepAscDesc xs@[_] = [xs]
sepAscDesc (x0 : y0 : xs0)
	| x0 <= y0 = asc y0 (x0 :) xs0
	| otherwise = desc y0 [x0] xs0
	where
	asc x s [] = [s [x]]
	asc x s xa@(y : xs)
		| x <= y = asc y (s . (x :)) xs
		| otherwise = s [x] : sepAscDesc xa
	desc x s [] = [x : s]
	desc x s xa@(y : xs)
		| x <= y = (x : s) : sepAscDesc xa
		| otherwise = desc y (x : s) xs

sample1 :: [Int]
sample1 = take 100 . randomRs (1, 100) $ mkStdGen 8

data Tree a = Leaf Int a | Node Int (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size = \case Leaf s _ -> s; Node s _ _ -> s

node :: Tree a -> Tree a -> Tree a
node l r = Node (size l + size r) l r

leaf :: [a] -> Tree [a]
leaf xs = Leaf (length xs) xs

pop :: State [a] (Maybe a)
pop = state $ \case [] -> (Nothing, []); (x : xs) -> (Just x, xs)

makeTreeTrial :: Int -> State [[a]] (Maybe (Tree [a]))
makeTreeTrial ms = fmap leaf <$> pop >>= \case
	Just l -> Just <$> if size l >= ms then pure l else do
		maybe l (node l) <$> makeTreeTrial (size l)
	Nothing -> pure Nothing

makeTree :: Int -> State [[a]] (Maybe (Tree [a]))
makeTree ms = fmap leaf <$> pop >>= \case
	Just lf -> Just <$> makeTreeWithLeft ms lf
	Nothing -> pure Nothing

makeTreeWithLeft :: Int -> Tree [a] -> State [[a]] (Tree [a])
makeTreeWithLeft ms l | size l >= ms = pure l
makeTreeWithLeft ms l = fmap leaf <$> pop >>= \case
	Just lf -> makeTreeWithLeft ms . node l =<< makeTreeWithLeft (size l) lf
	Nothing -> pure l
