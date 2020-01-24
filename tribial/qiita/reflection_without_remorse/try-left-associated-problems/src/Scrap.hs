{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Scrap where

import Control.Monad

data ItIo i a = Get (i -> IO (ItIo i a)) |  Done a

instance Functor (ItIo i) where
	f `fmap` Done x = Done $ f x
	f `fmap` Get k = Get $ (fmap f <$>) . k

instance Applicative (ItIo i) where
	pure = Done
	Done k <*> mx = k <$> mx
	Get k <*> mx = Get $ k >=> pure . (>>= (<$> mx))

instance Monad (ItIo i) where
	Done x >>= g = g x
	Get f >>= g = Get $ f >=> pure . (>>= g)

get :: ItIo i i
get = Get $ pure . pure

consume :: (i -> IO a) -> ItIo i a
consume k = Get $ \i -> Done <$> k i

get' :: Show i => ItIo i i
get' = consume $ \i -> i <$ print i

sample1, sample2 :: ItIo Int Int
sample1 = do
	x <- get'
	y <- get'
	pure $ x + y

sample2 = do
	x <- get'
	y <- get'
	z <- get'
	pure $ x * y * z

feedAllIo :: ItIo i a -> [i] -> IO (Maybe a)
feedAllIo (Done x) _ = pure $ Just x
feedAllIo _ [] = pure Nothing
feedAllIo (Get f) (h : t) = (`feedAllIo` t) =<< f h

{-
parIo :: IO (ItIo i a) -> IO (ItIo i b) -> ItIo i (ItIo i a, ItIo i b)
parIo l_ r_ = do
	l <- l_
	r <- r_
	case (l, r) of
		(Done _, _) -> Done (l, r)
		(_, Done _) -> Done (l, r)
		(Get f, Get g) -> get >>= \x -> parIo (f x) (g x)
		-}

{-
feedPartial :: Int -> ItIo i a -> IO (ItIo i (ItIo i a))
feedPartial n m | n < 1 = pure $ pure m
feedPartial _ m@(Done _) = pure $ pure m
feedPartial n (Get f) = get >>= \x -> feedPartial (n - 1) =<< f x
-}
