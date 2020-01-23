{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RwoR.PIteratee where

import Control.Monad

import RwoR.FTCQueue
import RwoR.PMonad

data PIt i a = Get (FTCQueue (PIt i) i a) | Done a

instance PMonad (PIt i) where
	pure' = Done
	Done x >>=. g = val g x
	Get f >>=. g = Get $ f >< g

get :: PIt i i
get = Get $ expr pure

pFeedAll :: PIt a b -> [a] -> Maybe b
pFeedAll (Done x) _ = Just x
pFeedAll _ [] = Nothing
pFeedAll (Get f) (h : t) = pFeedAll (val f h) t

pFeedPartial :: Int -> PIt i a -> PIt i (PIt i a)
pFeedPartial n m | n < 1 = pure m
pFeedPartial _ m@(Done _) = pure m
pFeedPartial n (Get f) = get >>= \x -> pFeedPartial (n - 1) (val f x)

pFeedPartialJoin :: Int -> PIt i a -> PIt i a
pFeedPartialJoin n = join . pFeedPartial n

pAddGet :: Int -> PIt Int Int
pAddGet x = get >>= \i -> pure (i + x)

pAddN :: Int -> PIt Int Int
pAddN n = foldl (>=>) pure (replicate n pAddGet) 0

tryPFeedPartialJoin :: Int -> Int -> PIt Int Int
tryPFeedPartialJoin m n = iterate (pFeedPartialJoin m) (pAddN $ 2 * m) !! n
