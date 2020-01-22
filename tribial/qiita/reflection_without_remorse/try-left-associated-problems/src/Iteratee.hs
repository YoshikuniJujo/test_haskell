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
