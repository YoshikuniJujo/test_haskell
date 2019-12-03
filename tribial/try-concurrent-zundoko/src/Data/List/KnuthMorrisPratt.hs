{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.KnuthMorrisPratt (matches, KmpState, initialState, nextState, found, takeWhile) where

import Prelude hiding (takeWhile)
import Control.Arrow

data KmpState a = KmpState {
	rootRep :: Rep [a],
	currentRep :: Rep [a] }

initialState :: Eq a => [a] -> KmpState a
initialState ws = KmpState root root
	where root = grep root Null ws

nextState :: Eq a => KmpState a -> a -> KmpState a
nextState st x = KmpState {
	rootRep = rootRep st,
	currentRep = step (rootRep st) (currentRep st) x }

found :: KmpState a -> Bool
found = ok . currentRep

takeWhile :: Eq a => KmpState a -> [a] -> ([a], Maybe (KmpState a))
takeWhile st [] = ([], Just st)
takeWhile st (x : xs)
	| ok $ currentRep st' = ([x], Nothing)
	| otherwise = (x :) `first` takeWhile st' xs
	where st' = nextState st x

data Rep a = Null | Node a (Rep a) (Rep a)

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = (fst <$>) . filter (ok . snd)
	. scanl (\(n, t) -> const (n + 1) &&& step root t) (0, root)
	where root = grep root Null ws

ok :: Rep [a] -> Bool
ok = \case Null -> False; Node vs _ _ -> null vs

step :: Eq a => Rep [a] -> Rep [a] -> a -> Rep [a]
step rt = op
	where
	op Null _ = rt
	op (Node [] l _) x = op l x
	op (Node (v : _) l r) x
		| v == x = r
		| otherwise = op l x

next :: Eq a => Rep [a] -> a -> Rep [a]
next t@Null _ = t
next t@(Node [] _ _) _ = t
next t@(Node (v : _) l _) x | v == x = next l x | otherwise = t

grep :: Eq a => Rep [a] -> Rep [a] -> [a] -> Rep [a]
grep _ l [] = Node [] l Null
grep rt l va@(v : vs) = Node va (next l v) (grep rt (step rt l v) vs)
