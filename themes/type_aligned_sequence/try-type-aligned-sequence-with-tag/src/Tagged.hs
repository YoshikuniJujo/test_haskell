{-# LANGUAGE BlockArguments, TupleSections, RankNTypes, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tagged (FTCQueue, tsingleton, Count, runCount, Tagged, mkTagged, parpar, apply, apply') where

import Control.Arrow

import FTCQueue
import Iteratee

data Tagged s m a b = Tagged Integer (a -> m b)
type TaggedFTCQueue s m = FTCQueue (Tagged s m)

newtype Count s a = Count { unCount :: Integer -> (a, Integer) }

instance Functor (Count s) where f `fmap` Count k = Count $ (f `first`) . k

instance Applicative (Count s) where
	pure x = Count $ (x ,)
	Count k <*> mx = Count $ uncurry unCount . ((<$> mx) `first`) . k

instance Monad (Count s) where
	Count k >>= f = Count $ uncurry unCount . (f `first`) . k

countup :: Count s Integer
countup = Count $ id &&& (+ 1)

apply :: Monad m => TaggedFTCQueue s m a b -> a -> m (Count s b)
apply fa x = case tviewl fa of
	TOne (Tagged _ f) -> pure <$> f x
	Tagged _ f :| fs -> apply fs =<< f x

apply' :: Monad m => Count s (TaggedFTCQueue s m a b) -> a -> Count s (m b)
apply' t x = foo $ fmap (`apply` x) t

foo :: Monad m => Count s (m (Count s a)) -> Count s (m a)
foo cnt = Count \c -> let (mcnt', c') = unCount cnt c in (, c') do
	cnt' <- mcnt'
	let	(r, _c'') = unCount cnt' c'
	pure r

mkTagged :: (a -> m b) -> Count s (Tagged s m a b)
mkTagged f = (`Tagged` f) <$> countup

runCount :: (forall s . Count s a) -> a
runCount m = fst $ m `unCount` 0

parpar :: Monad m => Tagged s m a b -> Tagged s m a b -> Count s (Tagged s m a (b, b))
parpar (Tagged i f) (Tagged j g)
	| i == j = do
		c <- countup
		pure $ Tagged c \x -> let y = f x in (,) <$> y <*> y
	| otherwise = mkTagged $ \x -> (,) <$> f x <*> g x

{-
par'' :: Monad m => FTCQueue (Tagged s m) a b -> FTCQueue (Tagged s m) a b -> Count s (FTCQueue (Tagged s m) a (b, b))
par'' fa ga = case (tviewl fa, tviewl ga) of
-}

data TaggedMonad s m a = forall x . TaggedMonad x (FTCQueue (Tagged s m) x a)

purec :: Monad m => a -> Count s (TaggedMonad s m a)
purec x = do
	p <- mkTagged pure
	pure $ TaggedMonad x $ tsingleton p

-- (>>=!) :: Monad m => TaggedMonad s m a -> (a -> TaggedMonad s m b) -> Count s (TaggedMonad s m b)

{-
instance Monad m => Functor (TaggedMonad s m) where
	f `fmap` TaggedMonad x fs = TaggedMonad x (fs |> mkTagged f)
	-}
