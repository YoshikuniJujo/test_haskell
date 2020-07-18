{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BaseSortState (baseSort, sample1) where

import Prelude hiding (head, tail)

import Control.Monad.Identity
import Control.Monad.State
import System.Random

baseSort :: Ord a => [a] -> [a]
baseSort = (sortSt `evalState`)

sortSt :: Ord a => State [a] [a]
sortSt = popSorted >>= \case
	Nothing -> pure []
	Just l -> mergeAll l popSorted

mergeAll :: Ord a => [a] -> State [a] (Maybe [a]) -> State [a] [a]
mergeAll l pop = pop >>= \case
	Nothing -> pure l
	Just r -> mergeAll (l `merge` r) (pair pop)

pair :: Ord a => State [a] (Maybe [a]) -> State [a] (Maybe [a])
pair pop = pop >>= \case
	Nothing -> pure Nothing
	Just l -> (<$> pop) \case
		Nothing -> Just l
		Just r -> Just $ l `merge` r

popSorted :: Ord a => State [a] (Maybe [a])
popSorted = pop2 >>= \case
	Zero -> pure Nothing
	One x -> pure $ Just [x]
	Two x y	| x <= y -> Just <$> asc y (x :)
		| otherwise -> Just <$> desc y [x]

asc :: Ord a => a -> ([a] -> [a]) -> State [a] [a]
asc x s = head >>= \case
	Just y	| x <= y -> tail >> asc y (s . (x :))
	_ -> pure $ s [x]

desc :: Ord a => a -> [a] -> State [a] [a]
desc x s = head >>= \case
	Just y	| x > y -> tail >> desc y (x : s)
	_ -> pure $ x : s

merge :: Ord a => [a] -> [a] -> [a]
xs `merge` [] = xs
[] `merge` ys = ys
xa@(x : xs) `merge` ya@(y : ys)
	| x <= y = x : xs `merge` ya
	| otherwise = y : xa `merge` ys

data Either012 a = Zero | One a | Two a a deriving Show

pop2 :: State [a] (Either012 a)
pop2 = StateT $ Identity . \case
	[] -> (Zero, []); [x] -> (One x, []); x : y : xs -> (Two x y, xs)

head :: State [a] (Maybe a)
head = gets \case [] -> Nothing; x : _ -> Just x

tail :: State [a] ()
tail = modify \case [] -> []; _ : xs -> xs

sample1 :: [Int]
sample1 = take 100 . randomRs (1, 100) $ mkStdGen 8
