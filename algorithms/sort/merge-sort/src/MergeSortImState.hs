{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSortImState where

import Prelude hiding (head, tail)

import Control.Monad.Identity
import Control.Monad.State
import System.Random

mergesortim :: Ord a => [a] -> [a]
mergesortim = (sortSt `evalState`)

sortSt :: Ord a => State [a] [a]
sortSt = maybe (pure []) ((list <$>) . sortStWithL Nothing) =<< popSorted

sortStWithL :: Ord a => Maybe Int -> LenList a -> State [a] (LenList a)
sortStWithL (Just ms) l | len l >= ms = pure l
sortStWithL ms l = popSorted >>= \case
	Nothing -> pure l
	Just lf -> sortStWithL ms . merge l =<< sortStWithL (Just $ len l) lf

popSorted :: Ord a => State [a] (Maybe (LenList a))
popSorted = pop2 >>= \case
	Zero -> pure Nothing
	One x -> pure . Just $ singleton x
	Two x y	| x <= y -> Just <$> asc y (x `cons`)
		| otherwise -> Just <$> desc y (singleton x)

asc :: Ord a => a -> (LenList a -> LenList a) -> State [a] (LenList a)
asc x s = head >>= \case
	Just y	| x <= y -> tail >> asc y (s . (x `cons`))
	_ -> pure . s $ singleton x

desc :: Ord a => a -> LenList a -> State [a] (LenList a)
desc x s = head >>= \case
	Just y	| x > y -> tail >> desc y (x `cons` s)
	_ -> pure $ x `cons` s

merge :: Ord a => LenList a -> LenList a -> LenList a
merge (LenList nl xs0) (LenList nr ys0) = LenList (nl + nr) $ xs0 `mrg` ys0
	where
	xs `mrg` [] = xs
	[] `mrg` ys = ys
	xa@(x : xs) `mrg` ya@(y : ys)
		| x <= y = x : xs `mrg` ya
		| otherwise = y : xa `mrg` ys

data Either012 a = Zero | One a | Two a a deriving Show

pop2 :: State [a] (Either012 a)
pop2 = state \case
	[] -> (Zero, []); [x] -> (One x, []); x : y : xs -> (Two x y, xs)

head :: State [a] (Maybe a)
head = gets \case [] -> Nothing; x : _ -> Just x

tail :: State [a] ()
tail = modify \case [] -> []; _ : xs -> xs

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

state :: (s -> (a, s)) -> State s a
state f = StateT $ Identity . f

sample1 :: [Int]
sample1 = take 100 . randomRs (1, 100) $ mkStdGen 8
