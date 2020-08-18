{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par (
	-- * Freer
	-- ** Type
	Freer, Fun,
	-- ** Pattern
	pattern Pure, pattern (:>>=), pattern (:=<<),
	-- ** Bind
	(>>>=), (=<<<),
	-- ** Apply
	app, appPar,
	-- * Tagged
	Tagged, runTagged, tag ) where

import Control.Arrow (first, (&&&))
import Control.Monad.Freer.Par.Sequence (Sequence(..), ViewL(..), (<|), (|>))
import Control.Monad.Freer.Par.Funable (
	Funable(..), Taggable(..), MaybeId(..), Boolean(..) )
import Control.Monad.Freer.Par.Internal.Id (Id(..))
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

---------------------------------------------------------------------------

-- * PARALLEL FREER
--	+ TYPE AND MONAD
--	+ PATTERN
--	+ BIND
--	+ APPLICATION
-- * TAGGED

---------------------------------------------------------------------------
-- PARALLEL FREER
---------------------------------------------------------------------------

-- TYPE AND MONAD

infixl 7 ::>>=

data Freer s sq (f :: (* -> *) -> * -> * -> *) t a =
	Pure_ a | forall x . t x ::>>= sq (f (Freer s sq f t)) x a

freer :: (a -> b) -> (forall x . t x -> sq (f (Freer s sq f t)) x a -> b) ->
	Freer s sq f t a -> b
freer p b = \case Pure_ x -> p x; t ::>>= k -> t `b` k

instance (Sequence sq, Funable f) => Functor (Freer s sq f t) where
	fmap f = freer (Pure_ . f) \t k -> t ::>>= k |> fun (Pure_ . f)

instance (Sequence sq, Funable f) => Applicative (Freer s sq f t) where
	pure = Pure_
	mf <*> mx = freer (<$> mx) (\m k -> m ::>>= k |> fun (<$> mx)) mf

instance (Sequence sq, Funable f) => Monad (Freer s sq f t) where
	m >>= f = freer f (\m' k -> m' ::>>= k |> fun f) m

newtype Fun s sq f t a b = Fun (sq (f (Freer s sq f t)) a b)

-- PATTERN

pattern Pure :: a -> Freer s sq f t a
pattern Pure x <- Pure_ x

{-# COMPLETE Pure, (:>>=) #-}

pattern (:>>=) :: t x -> Fun s sq f t x a -> Freer s sq f t a
pattern x :>>= k <- x ::>>= (Fun -> k)

{-# COMPLETE Pure, (:=<<) #-}

pattern (:=<<) :: Fun s sq f t x a -> t x -> Freer s sq f t a
pattern k :=<< x <- x ::>>= (Fun -> k)

-- BIND

infixl 7 >>>=

(>>>=) :: (Sequence sq, Funable f) =>
	t a -> (a -> Freer s sq f t b) -> Freer s sq f t b
m >>>= f = m ::>>= singleton (fun f)

infixr 7 =<<<

(=<<<) :: (Sequence sq, Funable f) =>
	(a -> Freer s sq f t b) -> t a -> Freer s sq f t b
(=<<<) = flip (>>>=)

-- APPLICATION

app, qApp :: (Sequence sq, Funable f) =>
--	sq (f (Freer s sq f t)) a b -> a -> Freer s sq f t b
	Fun s sq f t a b -> a -> Freer s sq f t b
app = qApp
Fun q `qApp` x = case viewl q of
	EmptyL -> pure x
	f :<| r -> case f $$ x of
		Pure_ y -> Fun r `qApp` y; tx ::>>= q' -> tx ::>>= q' >< r

appPar, qAppPar :: (Sequence sq, Funable f, Taggable f) =>
--	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a ->
	Fun s sq f t a b -> Fun s sq f t a b -> a ->
	(Freer s sq f t b, Freer s sq f t b)
appPar = qAppPar
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
			Pure_ y -> qAppParOpened tg r (unsafeCoerce r') y
			tx ::>>= p' -> (
				tx ::>>= p' >< r,
				tx ::>>= p' >< unsafeCoerce r' )
	_ -> error "never occur: no close tag"

---------------------------------------------------------------------------
-- TAGGED
---------------------------------------------------------------------------

newtype Tagged s a = Tagged { unTagged :: Natural -> (a, Natural) }

instance Functor (Tagged s) where f `fmap` Tagged k = Tagged $ (f `first`) . k

instance Applicative (Tagged s) where
	pure = Tagged . (,)
	Tagged k <*> mx = Tagged $ uncurry unTagged . ((<$> mx) `first`) . k

instance Monad (Tagged s) where
	Tagged k >>= f = Tagged $ uncurry unTagged . (f `first`) . k

runTagged :: (forall s . Tagged s a) -> a
runTagged (Tagged k) = fst $ k 0

tag :: (Sequence sq, Funable f, Taggable f) =>
	Freer s sq f t a -> Tagged s (Freer s sq f t a)
tag m@(Pure_ _) = pure m
tag (tx ::>>= fs) = (<$> Tagged (id &&& (+ 1))) \n ->
	let tg = Id n in tx ::>>= open tg <| (fs |> close tg)
