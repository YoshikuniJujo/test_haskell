{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes, ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par (
	-- * Freer
	Freer(Pure), Fun, pattern (:>>=), (>>>=), (=<<<), qApp, qAppPar,
	-- * Unique ID
	Unique, runUnique, tag ) where

import Control.Arrow ((&&&), first)
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Freer.Par.Sequence (Sequence(..), ViewL(..), (<|), (|>))
import Control.Monad.Freer.Par.Funable (
	Funable(..), Taggable(..), MaybeId(..), Boolean(..) )
import Control.Monad.Freer.Par.Internal.Id (Id(..))

---------------------------------------------------------------------------

-- * PARALLEL FREER
--	+ TYPE AND MONAD
--	+ APPLICATION
-- * UNIQUE ID

---------------------------------------------------------------------------
-- PARALLEL FREER
---------------------------------------------------------------------------

-- TYPE AND MONAD

data Freer s sq (f :: (* -> *) -> * -> * -> *) t a =
	Pure a | forall x . t x ::>>= sq (f (Freer s sq f t)) x a

{-# COMPLETE Pure, (:>>=) #-}

pattern (:>>=) :: t x -> Fun s sq f t x a -> Freer s sq f t a
pattern x :>>= k <- x ::>>= (Fun -> k)

infix 8 >>>=, =<<<

(>>>=) :: (Sequence sq, Funable f) =>
	t a -> (a -> Freer s sq f t b) -> Freer s sq f t b
m >>>= f = m ::>>= singleton (fun f)

(=<<<) :: (Sequence sq, Funable f) =>
	(a -> Freer s sq f t b) -> t a -> Freer s sq f t b
(=<<<) = flip (>>>=)

freer :: (a -> b) -> (forall x . t x -> sq (f (Freer s sq f t)) x a -> b) ->
	Freer s sq f t a -> b
freer f _ (Pure x) = f x; freer _ g (m ::>>= k) = g m k

instance (Sequence sq, Funable f) => Functor (Freer s sq f t) where
	fmap f = freer (Pure . f) \m k -> m ::>>= (k |> fun (Pure . f))

instance (Sequence sq, Funable f) => Applicative (Freer s sq f t) where
	pure = Pure
	mf <*> mx = freer (<$> mx) (\m k -> m ::>>= (k |> fun (<$> mx))) mf

instance (Sequence sq, Funable f) => Monad (Freer s sq f t) where
	m >>= f = freer f (\m' k -> m' ::>>= (k |> fun f)) m

-- APPLICATION

newtype Fun s sq f t a b = Fun (sq (f (Freer s sq f t)) a b)

qApp :: (Sequence sq, Funable f) =>
--	sq (f (Freer s sq f t)) a b -> a -> Freer s sq f t b
	Fun s sq f t a b -> a -> Freer s sq f t b
Fun q `qApp` x = case viewl q of
	EmptyL -> pure x
	f :<| r -> case f $$ x of
		Pure y -> Fun r `qApp` y; tx ::>>= q' -> tx ::>>= (q' >< r)

qAppPar :: (Sequence sq, Funable f, Taggable f) =>
--	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a ->
	Fun s sq f t a b -> Fun s sq f t a b -> a ->
	(Freer s sq f t b, Freer s sq f t b)
qAppPar (Fun p) (Fun q) x = case (viewl p, viewl q) of
	(t :<| r, t' :<| r') | J tg <- checkOpen t t' -> qAppParOpened tg r r' x
	_ -> (Fun p `qApp` x, Fun q `qApp` x)

qAppParOpened :: (Sequence sq, Funable f, Taggable f) => Id ->
	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a ->
	(Freer s sq f t b, Freer s sq f t b)
qAppParOpened tg p q x = case (viewl p, viewl q) of
	(t :<| r, t' :<| r') -> case (checkClose tg t, checkClose tg t') of
		(T, T) -> (Fun r `qApp` x, Fun r' `qApp` x)
		_ -> case t $$ x of
			Pure y -> qAppParOpened tg r (unsafeCoerce r') y
			tx ::>>= p' -> (
				tx ::>>= (p' >< r),
				tx ::>>= (p' >< unsafeCoerce r') )
	_ -> error "never occur: no close tag"

---------------------------------------------------------------------------
-- UNIQUE ID
---------------------------------------------------------------------------

newtype Unique s a = Unique { unUnique :: Natural -> (a, Natural) }

instance Functor (Unique s) where f `fmap` Unique k = Unique $ (f `first`) . k

instance Applicative (Unique s) where
	pure = Unique . (,)
	Unique k <*> mx = Unique $ uncurry unUnique . ((<$> mx) `first`) . k

instance Monad (Unique s) where
	Unique k >>= f = Unique $ uncurry unUnique . (f `first`) . k

runUnique :: (forall s . Unique s a) -> a
runUnique (Unique k) = fst $ k 0

tag :: (Sequence sq, Funable f, Taggable f) =>
	Freer s sq f t a -> Unique s (Freer s sq f t a)
tag m@(Pure _) = pure m
tag (tx ::>>= fs) = (<$> Unique (id &&& (+ 1))) \n ->
	let tg = Id n in tx ::>>= (open tg <| fs |> close tg)
