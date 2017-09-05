{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Free (Free(..)) where

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
	fmap f = go where
		go (Pure a) = Pure (f a)
		go (Free fa) = Free (go <$> fa)

instance Functor f => Applicative (Free f) where
	pure = Pure
	Pure a <*> Pure b = Pure $ a b
	Pure a <*> Free mb = Free $ fmap a <$> mb
	Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
	return = pure
	Pure a >>= f = f a
	Free m >>= f = Free ((>>= f) <$> m)
