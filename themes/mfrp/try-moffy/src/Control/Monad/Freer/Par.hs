{-# LANGUAGE ExistentialQuantification, GADTs, RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par (
	-- * Freer
	Freer(..), (>>>=), qApp, qAppPar,
	-- * Unique ID
	Unique, runUnique, tag ) where

import Control.Arrow
import Unsafe.Coerce
import Numeric.Natural

import Control.Monad.Freer.Par.Sequence
import Control.Monad.Freer.Par.Fun
import Control.Monad.Freer.Par.Internal.Id

data Freer s sq (f :: (* -> *) -> * -> * -> *) t a =
	Pure a | forall x . t x :>>= sq (f (Freer s sq f t)) x a

instance (Sequence sq, Fun f) => Functor (Freer s sq f t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (m :>>= k) = m :>>= (k |> fun (Pure . f))

instance (Sequence sq, Fun f) => Applicative (Freer s sq f t) where
	pure = Pure
	Pure f <*> mx = f <$> mx
	(m :>>= k) <*> mx = m :>>= (k |> fun (<$> mx))

instance (Sequence sq, Fun f) => Monad (Freer s sq f t) where
	Pure x >>= f = f x
	(m :>>= k) >>= f = m :>>= (k |> fun f)

(>>>=) :: (Sequence sq, Fun f) => t a -> (a -> Freer s sq f t b) -> Freer s sq f t b
m >>>= f = m :>>= singleton (fun f)

qApp :: (Sequence sq, Fun f) => sq (f (Freer s sq f t)) a b -> a -> Freer s sq f t b
q `qApp` x = case viewl q of
	EmptyL -> pure x
	f :<| r -> case f $$ x of
		Pure y -> r `qApp` y
		tx :>>= q' -> tx :>>= (q' >< r)

newtype Unique s a = Unique { unUnique :: Natural -> (a, Natural) }

instance Functor (Unique s) where f `fmap` Unique k = Unique $ (f `first`) . k

instance Applicative (Unique s) where
	pure = Unique . (,)
	Unique k <*> mx = Unique $ uncurry unUnique . ((<$> mx) `first`) . k

instance Monad (Unique s) where
	Unique k >>= f = Unique $ uncurry unUnique . (f `first`) . k

runUnique :: (forall s . Unique s a) -> a
runUnique (Unique k) = fst $ k 0

countup :: Unique s Natural
countup = Unique $ id &&& (+ 1)

tag :: (Sequence sq, Fun f, Taggable f) => Freer s sq f t a -> Unique s (Freer s sq f t a)
tag m@(Pure _) = pure m
tag (tx :>>= fs) = do
	tg <- countup
	pure $ tx :>>= (open (Id tg) <| fs |> close (Id tg))

qAppPar :: (Sequence sq, Fun f, Taggable f) =>
	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a -> (Freer s sq f t b, Freer s sq f t b)
qAppPar p q x = case (viewl p, viewl q) of
	(t :<| r, t' :<| r') -> case checkOpen t t' of
		J tg -> qAppParOpened tg r r' x
		N -> (p `qApp` x, q `qApp` x)
	_ -> (p `qApp` x, q `qApp` x)

qAppParOpened :: (Sequence sq, Fun f, Taggable f) => Id ->
	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a -> (Freer s sq f t b, Freer s sq f t b)
qAppParOpened tg p q x = case (viewl p, viewl q) of
	(t :<| r, t' :<| r') -> case (checkClose tg t, checkClose tg t') of
		(T, T) -> (r `qApp` x, r' `qApp` x)
		_ -> case t $$ x of
			Pure y -> qAppParOpened tg r (unsafeCoerce r') y
			tx :>>= p' -> (tx :>>= (p' >< r), tx :>>= (p' >< unsafeCoerce r'))
--			tx :>>= p' -> (tx :>>= (next tg <| p' >< r), tx :>>= (next tg <| p' >< unsafeCoerce r'))
	_ -> error "never occur: no close tag"
