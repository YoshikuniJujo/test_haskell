{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodensityIterateeProblem where

import Prelude hiding (abs)
import Control.Arrow
import Control.Monad

import Iteratee
import CodensityMonad

type ItCo i a = CodensityT (It i) a

getCo :: ItCo i i
getCo = rep get

parCo :: ItCo i a -> ItCo i b -> ItCo i (ItCo i a, ItCo i b)
parCo l r = (rep *** rep) <$> rep (par (abs l) (abs r))

parCoAll :: [ItCo i a] -> ItCo i [a]
parCoAll [] = pure []
parCoAll (i : is) = do
	(i', is') <- parCo i $ parCoAll is
	x <- i'; xs <- is'
	pure $ x : xs

applyCo :: ItCo i a -> [i] -> (a, [i])
applyCo = apply . abs

sample, sampleCo, sample2', sample2Co :: (Integer, [Integer])
sample = {-# SCC "NotCodensityTransformed" #-}
	sum `first` (parAll (replicate 1000000 sample1) `apply` [2, 3, 4, 5, 6])
sampleCo = {-# SCC "CodensityTransformed" #-}
	sum `first` (parCoAll (replicate 1000000 $ rep sample1) `applyCo` [2, 3, 4, 5, 6])
sample2' = {-# SCC "NotCodensityTransformed2" #-}
	sum `first` (parAll (replicate 1000000 sample2) `apply` [2, 3, 4, 5, 6])
sample2Co = {-# SCC "CodensityTransformed2" #-}
	sum `first` (parCoAll (replicate 1000000 $ rep sample2) `applyCo` [2, 3, 4, 5, 6])

sumInput :: Int -> (Integer -> It Integer Integer)
sumInput n = (foldl (>=>) return (replicate (n - 1) f))
	where f x = get >>= return . (+ x)

sumInputCo :: Int -> (Integer -> ItCo Integer Integer)
sumInputCo n = (foldl (>=>) return (replicate (n - 1) f))
	where f x = getCo >>= return . (+ x)

sample3, sample3Co :: (Integer, [Integer])
sample3 = {-# SCC "NotCodensityTransformed3" #-}
	sum `first` (parAll (replicate 200 $ sumInput 500 0) `apply` [1 .. 500])
sample3Co = {-# SCC "CodensityTransformed3" #-}
	sum `first` (parCoAll (replicate 500 $ sumInputCo 1000 0) `applyCo` [1 .. 1000])

sample4Co :: Int -> (Integer, [Integer])
sample4Co n = sum `first` ((parCoAll $ (flip sumInputCo 0) <$> [1 .. n]) `applyCo` [1 .. fromIntegral n])

feedAllCo :: ItCo i a -> [i] -> Maybe a
feedAllCo = feedAll . abs

addNCo :: Int -> ItCo Int Int
addNCo n = foldl (>=>) pure (replicate n $ rep . addGet) 0

bindIfGetCo :: ItCo i a -> (a -> ItCo i a) -> ItCo i a
bindIfGetCo m f = rep $ abs m >>= abs . f

connectIfGetCo :: (a -> ItCo i b) -> (b -> ItCo i b) -> (a -> ItCo i b)
connectIfGetCo f g x = f x `bindIfGetCo` g

feedPartialCo :: Int -> ItCo i a -> ItCo i (ItCo i a)
feedPartialCo n m = rep <$> rep (feedPartial n $ abs m)

tryFeedPartialCo :: ItCo Int Int
tryFeedPartialCo = do
	m <- feedPartialCo 2000 $ addNCo 4000
	m

feedPartialJoinCo :: Int -> ItCo i a -> ItCo i a
feedPartialJoinCo n = join . feedPartialCo n

tryFeedPartialJoinCo :: Int -> ItCo Int Int
tryFeedPartialJoinCo n = iterate (feedPartialJoinCo 2000) (addNCo 4000) !! n
