{-# LANGUAGE BlockArguments, TupleSections, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UniqueMonad (CountT, runCountT, TaggedMonad, mkMonad, parMonad, apply) where

import Control.Arrow
import Control.Monad.Trans

newtype CountT s m a = CountT { unCountT :: Integer -> m (a, Integer) }

instance Functor m => Functor (CountT s m) where
	f `fmap` m = CountT \c -> (f `first`) <$> unCountT m c

instance Monad m => Applicative (CountT s m) where
	pure x = CountT $ pure . (x ,)
	mf <*> mx = CountT \c ->
		uncurry unCountT . ((<$> mx) `first`) =<< unCountT mf c

instance Monad m => Monad (CountT s m) where
	m >>= f = CountT \c -> uncurry unCountT . (f `first`) =<< unCountT m c

instance MonadTrans (CountT s) where
	lift m = CountT $ \c -> (, c) <$> m

countup :: Applicative m => CountT s m Integer
countup = CountT \c -> pure (c, c + 1)

data TaggedMonad s m a b = TaggedMonad Integer (a -> m b)

apply :: Monad m => TaggedMonad s m a b -> a -> CountT s m b
apply (TaggedMonad _ f) x = lift $ f x

mkMonad :: Applicative m => (a -> m b) -> CountT s m (TaggedMonad s m a b)
mkMonad m = (`TaggedMonad` m) <$> countup

runCountT :: Functor m => (forall s . CountT s m a) -> m a
runCountT m = fst <$> m `unCountT` 0

class ParMonad b c where
	parMonad :: Monad m => TaggedMonad s m a b -> TaggedMonad s m a c ->
		CountT s m (TaggedMonad s m a (b, c))

instance {-# OVERLAPPABLE #-} ParMonad b c where
	parMonad (TaggedMonad _ f) (TaggedMonad _ g) =
		mkMonad $ \x -> (,) <$> f x <*> g x

instance ParMonad b b where
	parMonad (TaggedMonad i f) (TaggedMonad j g)
		| i == j = do
			c <- countup
			pure $ TaggedMonad c \x -> let y = f x in (,) <$> y <*> y
		| otherwise = mkMonad $ \x -> (,) <$> f x <*> g x
