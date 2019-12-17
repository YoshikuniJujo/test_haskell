{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Cont where

newtype Cont r a = C { rC :: (a -> r) -> r }

instance Functor (Cont r) where fmap f (C m) = C \k -> m $ k . f

instance Applicative (Cont r) where
	pure a = C \k -> k a
	C kf <*> C kx = C \k -> kf \f -> kx \x -> k $ f x

instance Monad (Cont r) where
	C m >>= f = C \k -> m \a -> rC (f a) k

pcreflect :: Monad m => m a -> Cont (m d) a
pcreflect m = C (\k -> m >>= k)

-- pcreify :: Monad m => (forall d . Cont (m d) a) -> m a
pcreify :: Monad m => Cont (m a) a -> m a
pcreify t = rC t return
