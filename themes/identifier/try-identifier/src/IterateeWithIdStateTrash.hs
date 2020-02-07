{-# LANGUAGE BlockArguments, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IterateeWithIdStateTrash where

import Control.Concurrent
import System.IO.Unsafe

newtype Iteratee a b = Iteratee { unIteratee :: Integer -> (DoneGet a b, Integer) }

data DoneGet a b = Done b | Get Integer (a -> Iteratee a b)

instance Functor (Iteratee a) where
	f `fmap` Iteratee k = Iteratee \c -> let (dg, c') = k c in case dg of
		Done x -> (Done $ f x, c')
		Get _ j -> (Get c' $ (f <$>) . j, c' + 1)

instance Applicative (Iteratee a) where
	pure x = Iteratee \c -> (Done x, c)
	mf <*> mx = (<$> mx) =<< mf

instance Monad (Iteratee a) where
	Iteratee k >>= f = Iteratee \c -> let (dg, c') = k c in case dg of
		Done x -> unIteratee (f x) c'
		Get _ j -> (Get c' $ (f =<<) . j, c' + 1)

get :: Iteratee a a
get = Iteratee $ \c -> (Get c \x -> Iteratee \c' -> (Done x, c'), c + 1)

apply :: Iteratee a b -> Integer -> [a] -> Maybe b
apply (Iteratee k) c xa = case (k c, xa) of
	((Done x, _), _) -> Just x
	((Get _ _, _), []) -> Nothing
	((Get _ f, c'), x : xs) -> apply (f x) c' xs

sample1 :: Iteratee Integer Integer
sample1 = do
	x <- get
	y <- get
	z <- get
	pure $ unsafePerformIO $ threadDelay 1000000 >> pure (x + y + z)

class Par b c where
	par :: Iteratee a b -> Iteratee a c -> Iteratee a (Iteratee a b, Iteratee a c)

instance {-# OVERLAPPABLE #-} Par b c where
	par l@(Iteratee k) r@(Iteratee j) = Iteratee \c -> let
		(dgl, c') = k c; (dgr, c'') = j c' in case (dgl, dgr) of
			(Get _ f, Get _ g) -> unIteratee (get >>= par <$> f <*> g) c''
			_ -> unIteratee (pure (l, r)) c''

{-
instance Par b b where
	par l@(Iteratee k) r@(Iteratee j) = Iteratee \c -> let
		(dgl, c') = k
		-}

it1, it2 :: Iteratee Integer Integer
Just (it1, it2) = apply (par sample1 sample1) 0 [3, 4, 5]

value1, value2 :: Maybe Integer
value1 = apply it1 0 []
value2 = apply it2 0 []
