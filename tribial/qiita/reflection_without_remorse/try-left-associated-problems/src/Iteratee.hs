{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iteratee where

import Control.Monad

data It i a = Get (i -> It i a) | Done a

instance Functor (It i) where
	f `fmap` Done x = Done $ f x
	f `fmap` Get k = Get $ (f <$>) . k

instance Applicative (It i) where
	pure = Done
	Done k <*> mx = k <$> mx
	Get k <*> mx = Get $ k >=> (<$> mx)

instance Monad (It i) where
	Done x >>= g = g x
	Get f >>= g = Get $ f >=> g

get :: It i i
get = Get pure

bindIfGet :: It i a -> (a -> It i a) -> It i a
bindIfGet m@(Done _) _ = m
bindIfGet m f = m >>= f

connectIfGet :: (a -> It i b) -> (b -> It i b) -> (a -> It i b)
connectIfGet f g x = f x `bindIfGet` g

addN' :: Int -> It Int Int
addN' n = foldl connectIfGet pure (replicate n addGet) 0

done :: It i a -> Maybe a
done (Done x) = Just x
done _ = Nothing

par :: It i a -> It i b -> It i (It i a, It i b)
par l r	| Done _ <- l = Done (l, r)
	| Done _ <- r = Done (l, r)
	| Get f <- l, Get g <- r = get >>= \x -> par (f x) (g x)

parAll :: [It i a] -> It i [a]
parAll [] = pure []
-- parAll [i] = (: []) <$> i
parAll (i : is) = do
	(i', is') <- par i $ parAll is
	x <- i'; xs <- is'
	pure $ x : xs

sample1, sample2 :: It Integer Integer
sample1 = do
	x <- get
	y <- get
	pure $ x + y

sample2 = do
	x <- get
	y <- get
	z <- get
	pure $ x * y * z

apply :: It i a -> [i] -> (a, [i])
apply (Done x) is = (x, is)
apply (Get f) (i : is) = apply (f i) is
apply _ [] = error "not enough inputs"

addGet :: Int -> It Int Int
addGet x = get >>= \i -> pure (i + x)

addN, addNR :: Int -> It Int Int
addN n = foldl (>=>) pure (replicate n addGet) 0
addNR n = foldr (>=>) pure (replicate n addGet) 0

feedAll :: It a b -> [a] -> Maybe b
feedAll (Done x) _ = Just x
feedAll _ [] = Nothing
feedAll (Get f) (h : t) = feedAll (f h) t

feedPartial :: Int -> It i a -> It i (It i a)
feedPartial n m | n < 1 = pure m
feedPartial _ m@(Done _) = pure m
feedPartial n (Get f) = get >>= \x -> feedPartial (n - 1) (f x)

feedPartialJoin :: Int -> It i a -> It i a
feedPartialJoin n = join . feedPartial n

tryFeedPartial, tryFeedPartialR :: It Int Int
tryFeedPartial = do
	m <- feedPartial 2000 $ addN 4000
	m

tryFeedPartialR = do
	m <- feedPartial 2000 $ addNR 4000
	m

tryFeedPartialJoinR :: It Int Int
tryFeedPartialJoinR = iterate (feedPartialJoin 2000) (addN 4000)  !! 100
