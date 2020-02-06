{-# LANGUAGE BlockArguments, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IterateeWithIdIo where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe

globalCounter :: TVar Integer
globalCounter = unsafePerformIO $ newTVarIO 0

data Tick = Tick deriving Show

counter :: Tick -> Integer
counter _ = unsafePerformIO $ atomically do
	c <- readTVar globalCounter
	writeTVar globalCounter (c + 1)
	pure c

data Iteratee a b = Done b | Get Integer (a -> Iteratee a b)

instance Functor (Iteratee a) where
	f `fmap` m = pure . f =<< m

instance Applicative (Iteratee a) where
	pure = Done
	mf <*> mx = mf >>= (<$> mx)

instance Monad (Iteratee a) where
	Done x >>= f = f x
	Get _ k >>= f = Get (counter Tick) $ k >=> f

get :: Iteratee a a
get = Get (counter Tick) Done

apply :: Iteratee a b -> [a] -> Maybe b
Done x `apply` _ = Just x
Get _ _ `apply` [] = Nothing
Get _ f `apply` (x : xs) = f x `apply` xs

sample1, sample2 :: Iteratee Integer Integer
sample1 = do
	x <- get
	y <- get
	z <- unsafePerformIO $ let Get i _ = get in print i >> pure get
	pure $ unsafePerformIO $ threadDelay 1000000 >> pure (x + y + z)

sample2 = do
	x <- get
	y <- get
	z <- unsafePerformIO $ let Get i _ = get in print i >> pure get
	pure $ unsafePerformIO $ threadDelay 1000000 >> pure (x * y * z)

class Par b c where
	par :: Iteratee a b -> Iteratee a c -> Iteratee a (Iteratee a b, Iteratee a c)

instance {-# OVERLAPPABLE #-} Par b c where
	par (Get _ f) (Get _ g) = get >>= par <$> f <*> g
	par l r = pure (l, r)

instance Par b b where
	par (Get i f) (Get j g) = get >>= \x -> let l = f x in par l if i == j then l else g x
	par l r = pure (l, r)

value1, value2 :: Integer
Just (Done value1, Done value2) = par sample1 sample1 `apply` [3, 4, 5]
