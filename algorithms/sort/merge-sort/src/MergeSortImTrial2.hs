{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSortImTrial2 where

import Prelude hiding (head, tail)

import Control.Monad.Identity
import Control.Monad.State
import System.Random

state :: (s -> (a, s)) -> State s a
state f = StateT $ Identity . f

data Either012 a = Zero | One a | Two a a deriving Show

pop2 :: State [a] (Either012 a)
pop2 = state \case
	[] -> (Zero, []); [x] -> (One x, []); x : y : xs -> (Two x y, xs)

head :: State [a] (Maybe a)
head = gets \case [] -> Nothing; x : _ -> Just x

tail :: State [a] ()
tail = modify \case [] -> []; _ : xs -> xs

popSorted :: Ord a => State [a] (Maybe [a])
popSorted = pop2 >>= \case
	Zero -> pure Nothing
	One x -> pure $ Just [x]
	Two x y	| x <= y -> Just <$> asc y (x :)
		| otherwise -> Just <$> desc y [x]

asc :: Ord a => a -> ([a] -> [a]) -> State [a] [a]
asc x s = head >>= \case
	Nothing -> pure $ s [x]
	Just y	| x <= y -> tail >> asc y (s . (x :))
		| otherwise -> pure $ s [x]

desc :: Ord a => a -> [a] -> State [a] [a]
desc x s = head >>= \case
	Nothing -> pure $ x : s
	Just y	| x <= y -> pure $ x : s
		| otherwise -> tail >> desc y (x : s)

sample1 :: [Int]
sample1 = take 100 . randomRs (1, 100) $ mkStdGen 8

data Tree a = Leaf Int a | Node Int (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size = \case Leaf s _ -> s; Node s _ _ -> s

leaf :: [a] -> Tree [a]
leaf xs = Leaf (length xs) xs

node :: Tree a -> Tree a -> Tree a
node l r = Node (size l + size r) l r

makeTree :: Ord a => Int -> State [a] (Maybe (Tree [a]))
makeTree ms = fmap leaf <$> popSorted >>= \case
	Nothing -> pure Nothing
	Just lf -> Just <$> makeTreeWithLeft ms lf

makeTreeWithLeft :: Ord a => Int -> Tree [a] -> State [a] (Tree [a])
makeTreeWithLeft ms l | size l >= ms = pure l
makeTreeWithLeft ms l = fmap leaf <$> popSorted >>= \case
	Nothing -> pure l
	Just lf -> makeTreeWithLeft ms . node l =<< makeTreeWithLeft (size l) lf
