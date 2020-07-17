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

popSorted :: Ord a => State [a] (Maybe (LenList a))
popSorted = pop2 >>= \case
	Zero -> pure Nothing
	One x -> pure . Just $ singleton x
	Two x y	| x <= y -> Just <$> asc y (x `cons`)
		| otherwise -> Just <$> desc y (singleton x)

asc :: Ord a => a -> (LenList a -> LenList a) -> State [a] (LenList a)
asc x s = head >>= \case
	Nothing -> pure . s $ singleton x
	Just y	| x <= y -> tail >> asc y (s . (x `cons`))
		| otherwise -> pure . s $ singleton x

desc :: Ord a => a -> LenList a -> State [a] (LenList a)
desc x s = head >>= \case
	Nothing -> pure $ x `cons` s
	Just y	| x <= y -> pure $ x `cons` s
		| otherwise -> tail >> desc y (x `cons` s)

sample1 :: [Int]
sample1 = take 100 . randomRs (1, 100) $ mkStdGen 8

data Tree a = Leaf Int a | Node Int (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size = \case Leaf s _ -> s; Node s _ _ -> s

leaf :: LenList a -> Tree [a]
leaf (LenList n xs) = Leaf n xs

node :: Tree a -> Tree a -> Tree a
node l r = Node (size l + size r) l r

makeTree :: Ord a =>
	(t -> Int) -> (LenList a -> t) -> (t -> t -> t) -> State [a] (Maybe t)
makeTree sz atm nd = fmap atm <$> popSorted >>= \case
	Nothing -> pure Nothing
	Just lf -> Just <$> makeTreeWithLeft sz atm nd Nothing lf

makeTreeWithLeft :: Ord a =>
	(t -> Int) -> (LenList a -> t) -> (t -> t -> t) -> Maybe Int -> t -> State [a] t
makeTreeWithLeft sz _ _ (Just ms) l | sz l >= ms = pure l
makeTreeWithLeft sz atm nd ms l = fmap atm <$> popSorted >>= \case
	Nothing -> pure l
	Just lf -> makeTreeWithLeft sz atm nd ms . nd l =<< makeTreeWithLeft sz atm nd (Just $ sz l) lf

data LenList a = LenList Int [a] deriving Show

empty :: LenList a
empty = LenList 0 []

cons :: a -> LenList a -> LenList a
cons x (LenList n xs) = LenList (n + 1) (x : xs)

len :: LenList a -> Int
len (LenList n _) = n

list :: LenList a -> [a]
list (LenList _ xs) = xs

singleton :: a -> LenList a
singleton x = LenList 1 [x]

merge :: Ord a => LenList a -> LenList a -> LenList a
merge (LenList nl xs0) (LenList nr ys0) = LenList (nl + nr) $ xs0 `mrg` ys0
	where
	xs `mrg` [] = xs
	[] `mrg` ys = ys
	xa@(x : xs) `mrg` ya@(y : ys)
		| x <= y = x : xs `mrg` ya
		| otherwise = y : xa `mrg` ys

mergesortim :: Ord a => [a] -> [a]
mergesortim = maybe [] list . (makeTree len id merge `evalState`)
