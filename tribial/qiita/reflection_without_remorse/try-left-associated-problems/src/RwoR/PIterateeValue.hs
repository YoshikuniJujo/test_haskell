{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RwoR.PIterateeValue where

import Control.Arrow
import Control.Monad

import RwoR.FTCQueue
import RwoR.PMonad

data PItv i o a = Get o (FTCQueue (PItv i o) i a) | Done a

instance PMonad (PItv i o) where
	pure' = Done
	Done x >>=. g = val g x
	Get o f >>=. g = Get o $ f >< g

getv :: o -> PItv i o i
getv o = Get o $ expr pure

pFeedAll :: PItv i o a -> [i] -> ([o], Maybe a)
pFeedAll (Done x) _ = ([], Just x)
pFeedAll (Get o _) [] = ([o], Nothing)
pFeedAll (Get o f) (h : t) = (o :) `first` pFeedAll (val f h) t

pFeedPartial :: Int -> PItv i o a -> PItv i o (PItv i o a)
pFeedPartial n m | n < 1 = pure m
pFeedPartial _ m@(Done _) = pure m
pFeedPartial n (Get o f) = getv o >>= \x -> pFeedPartial (n - 1) (val f x)

pAddGet :: Int -> PItv Int Int Int
pAddGet x = getv x >>= \i -> pure (i + x)

pAddN, pAddNR :: Int -> PItv Int Int Int
pAddN n = foldl (>=>) pure (replicate n pAddGet) 0
pAddNR n = foldr (>=>) pure (replicate n pAddGet) 0

tryRwoR, tryRwoRR :: [Int]
tryRwoR = fst $ pFeedPartial 2000000 (pAddN 4000000) `pFeedAll` [1 .. 10]
tryRwoRR = fst $ pFeedPartial 2000000 (pAddNR 4000000) `pFeedAll` [1 .. 10]
