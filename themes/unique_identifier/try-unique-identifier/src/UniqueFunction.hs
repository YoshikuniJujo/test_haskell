{-# LANGUAGE BlockArguments, MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UniqueFunction (Count, runCount, Fun, mkFun, par, apply) where

import Control.Arrow ((&&&))

newtype Count s a = Count { unCount :: Integer -> (a, Integer) }

instance Functor (Count s) where
	f `fmap` Count k = Count \c -> let (x, c') = k c in (f x, c')

instance Applicative (Count s) where
	pure x = Count \c -> (x, c)
	Count k <*> mx = Count \c -> let (f, c') = k c in unCount (f <$> mx) c'

instance Monad (Count a) where
	Count k >>= f = Count \c -> let (x, c') = k c in unCount (f x) c'

countup :: Count s Integer
countup = Count \c -> (c, c+1)

data Fun s a b = Fun Integer (a -> b)

apply :: Fun s a b -> a -> Count s b
apply (Fun _ f) x = pure $ f x

mkFun :: (a -> b) -> Count s (Fun s a b)
mkFun f = (`Fun` f) <$> countup

class Par b c where par :: Fun s a b -> Fun s a c -> Count s (Fun s a (b, c))

instance {-# OVERLAPPABLE #-} Par b c where
	par (Fun _ f) (Fun _ g) = mkFun $ f &&& g

instance Par b b where
	par (Fun i f) (Fun j g)
		| i == j = do
			c <- countup
			pure $ Fun c \x -> let y = f x in (y, y)
		| otherwise = mkFun $ f &&& g

runCount :: (forall s . Count s a) -> a
runCount m = fst $ m `unCount` 0
