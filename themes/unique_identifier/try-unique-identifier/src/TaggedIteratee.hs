{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TaggedIteratee where

import FTCQueue

data It s i a = It { unIt :: Integer -> (DoneOrGet s i (a, Integer)) }
data DoneOrGet s i a = Done a | Get (TaggedExp s (DoneOrGet s i) i a)

instance Functor (It s i) where
	f `fmap` m = pure . f =<< m

instance Applicative (It s i) where
	pure x = It \c -> Done (x, c)
	(<*>) = (. flip (<$>)) . (>>=)

instance Monad (It s i) where
	It k >>= f = It \c -> case k c of
		Done (x, c') -> unIt (f x) c'
		Get k' -> Get $ k' |> Tagged c (\(x, c') -> unIt (f x) (c' + 1))

applyIt :: (forall s . It s i a) -> [i] -> Maybe a
applyIt (It k) ia = fst <$> applyDg (k 0) ia

applyIt' :: (forall s . It s i a) -> [i] -> Maybe (a, Integer)
applyIt' (It k) ia = applyDg (k 0) ia

applyDg :: DoneOrGet s i a -> [i] -> Maybe a
applyDg (Done x) _ = Just x
applyDg (Get _) [] = Nothing
applyDg (Get te) (i : is) = val te i `applyDg` is

get :: It s i i
get = It \c -> Get . tsingleton . Tagged c $ Done . (, c + 1)

showTagsIt :: (forall s . It s i a) -> [Integer]
showTagsIt (It k) = showTagsDg $ k 0

showTagsDg :: DoneOrGet s i a -> [Integer]
showTagsDg (Done _) = []
showTagsDg (Get te) = showTagsTe te

showTagsTe :: TaggedExp s m a b -> [Integer]
showTagsTe te = case tviewl te of
	TOne (Tagged tg _) -> [tg]
	Tagged tg _ :| t -> tg : showTagsTe t

{-
countup :: It s i Integer
countup = It \c -> Done (c, c + 1)
-}

{-

-- instance {-# INCOHERENT #-} Functor (CountT s (It s i)) where
--	f `fmap` m = m >>= expr (puf

class PMonad m where
	return' :: a -> m a
	(>>=^) :: m a -> TaggedExp s m a b -> m b

-- instance {-# INCOHERENT #-} PMonad m => Functor (CountT s m) where
instance {-# INCOHERENT #-} Functor (CountT s (It s i)) where
	f `fmap` m = m >>=^ expr (return' . f)

-}

{-
class PMonad m where
	return' :: a -> m a
	(>>=^) :: m a -> TaggedExp s m a b -> m b

instance PMonad (DoneOrGet s i) where
	return' = Done
--	(>>=^) :: forall s i a b . DoneOrGet s i a -> TaggedExp s (DoneOrGet s i) a b -> DoneOrGet s i b
	(>>=^) :: forall s i a b . DoneOrGet s i a -> TaggedExp s (DoneOrGet s i) a b -> DoneOrGet s i b
	Done x >>=^ f = val f x
	Get k >>=^ f = Get (k >< f)
	-}

(>>=^) :: DoneOrGet s i x -> TaggedExp s (DoneOrGet s i) x a -> DoneOrGet s i a
Done x >>=^ f = val f x
Get k >>=^ f = Get (k >< f)

val :: TaggedExp s (DoneOrGet s i) a b -> (a -> DoneOrGet s i b)
val q = case tviewl q of
	TOne (Tagged _ f) -> f
	Tagged _ h :| t -> \x -> h x >>=^ t

{-
expr :: Applicative m => (a -> m b) -> CountT s m (TaggedExp s m a b)
expr f = taggedMonadToExp <$> mkMonad f
-}
