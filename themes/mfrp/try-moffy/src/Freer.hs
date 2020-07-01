{-# LANGUAGE ExistentialQuantification, GADTs, RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (
	Freer(..), (>>>=), qApp, qAppPar,
	Count, runCount, addTag, Fun(..), Tag(..), Tg(..), MaybeTg(..), Boolean(..)
	) where

import Control.Arrow
import Unsafe.Coerce

import Sequence

data Freer s sq (f :: (* -> *) -> * -> * -> *) t a = Pure a | forall x . t x :>>= sq (f (Freer s sq f t)) x a

class Fun f where
	fun :: (a -> m b) -> f m a b
	($$) :: Applicative m => f m a b -> a -> m b

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

newtype Count s a = Count { unCount :: Integer -> (a, Integer) }

instance Functor (Count s) where f `fmap` Count k = Count $ (f `first`) . k

instance Applicative (Count s) where
	pure = Count . (,)
	Count k <*> mx = Count $ uncurry unCount . ((<$> mx) `first`) . k

instance Monad (Count s) where
	Count k >>= f = Count $ uncurry unCount . (f `first`) . k

runCount :: (forall s . Count s a) -> a
runCount (Count k) = fst $ k 0

countup :: Count s Integer
countup = Count $ id &&& (+ 1)

data MaybeTg a b c where
	N :: MaybeTg a b c
	J :: Tg -> MaybeTg a a a

data Boolean a b where
	F :: Boolean a b
	T :: Boolean a a

data Tg = Tg Integer Integer deriving Show

class Tag (t :: (* -> *) -> * -> * -> *) where
	open :: Integer -> t m a a
	next :: Tg -> t m a a
	close :: Integer -> t m a a
	checkOpen :: t m a b -> t m a c -> MaybeTg a b c
	checkClose :: Tg -> t m a b -> Boolean a b

addTag :: (Sequence sq, Fun f, Tag f) => Freer s sq f t a -> Count s (Freer s sq f t a)
addTag m@(Pure _) = pure m
addTag (tx :>>= fs) = do
	tg <- countup
	pure $ tx :>>= (open tg <| fs |> close tg)

qAppPar :: (Sequence sq, Fun f, Tag f) =>
	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a -> (Freer s sq f t b, Freer s sq f t b)
qAppPar p q x = case (viewl p, viewl q) of
	(t :<| r, t' :<| r') -> case checkOpen t t' of
		J tg -> qAppParOpened tg r r' x
		N -> (p `qApp` x, q `qApp` x)
	_ -> (p `qApp` x, q `qApp` x)

qAppParOpened :: (Sequence sq, Fun f, Tag f) => Tg ->
	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a -> (Freer s sq f t b, Freer s sq f t b)
qAppParOpened tg p q x = case (viewl p, viewl q) of
	(t :<| r, t' :<| r') -> case (checkClose tg t, checkClose tg t') of
		(T, T) -> (r `qApp` x, r' `qApp` x)
		_ -> case t $$ x of
			Pure y -> qAppParOpened tg r (unsafeCoerce r') y
			tx :>>= p' -> (tx :>>= (p' >< r), tx :>>= (p' >< unsafeCoerce r'))
--			tx :>>= p' -> (tx :>>= (next tg <| p' >< r), tx :>>= (next tg <| p' >< unsafeCoerce r'))
	_ -> error "never occur: no close tag"
