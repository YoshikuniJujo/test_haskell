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
import Control.Monad.Freer.Par.Sequence (Sequence(..), ViewL(..), (|>), mapS)
import Control.Monad.Freer.Par.Funable (Funable(..), Taggable(..))
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
	mf <*> (flip (<$>) -> ax) = freer ax (\t -> (t ::>>=) . (|> fun ax)) mf

instance (Sequence sq, Funable f) => Monad (Freer s sq f t) where
	m >>= f = freer f (\t -> (t ::>>=) . (|> fun f)) m

newtype Fun s sq f t a b = Fun (sq (f (Freer s sq f t)) a b)

-- PATTERN

pattern Pure :: a -> Freer s sq f t a
pattern Pure x <- Pure_ x

{-# COMPLETE Pure, (:>>=) #-}

pattern (:>>=) :: t x -> Fun s sq f t x a -> Freer s sq f t a
pattern t :>>= k <- t ::>>= (Fun -> k)

{-# COMPLETE Pure, (:=<<) #-}

pattern (:=<<) :: Fun s sq f t x a -> t x -> Freer s sq f t a
pattern k :=<< t <- t ::>>= (Fun -> k)

-- BIND

infixl 7 >>>=

(>>>=) :: (Sequence sq, Funable f) =>
	t a -> (a -> Freer s sq f t b) -> Freer s sq f t b
(>>>=) m = (m ::>>=) . singleton . fun

infixr 7 =<<<

(=<<<) :: (Sequence sq, Funable f) =>
	(a -> Freer s sq f t b) -> t a -> Freer s sq f t b
(=<<<) = flip (>>>=)

-- APPLICATION

app :: (Sequence sq, Funable f) => Fun s sq f t a b -> a -> Freer s sq f t b
Fun fa `app` x = case viewl fa of
	EmptyL -> pure x
	f :<| fs -> case f $$ x of
		Pure_ y -> Fun fs `app` y; t ::>>= k -> t ::>>= k >< fs

appPar :: (Sequence sq, Funable f, Taggable f) =>
	Fun s sq f t a b -> Fun s sq f t a b -> a ->
	(Freer s sq f t b, Freer s sq f t b)
appPar (Fun l) (Fun r) = appParOpened l r
{-
appPar fl@(Fun l) fr@(Fun r) x = case (viewl l, viewl r) of
	(f :<| fs, g :<| gs) | J tg <- checkOpen f g -> appParOpened tg fs gs x
	_ -> (fl `app` x, fr `app` x)
	-}

appParOpened :: (Sequence sq, Funable f, Taggable f) =>
	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a ->
	(Freer s sq f t b, Freer s sq f t b)
appParOpened l r x = case (viewl l, viewl r) of
	(EmptyL, _) -> (Fun l `app` x, Fun r `app` x)
	(_, EmptyL) -> (Fun l `app` x, Fun r `app` x)
	(f :<| fs, g :<| gs)
		| Just tg <- getTag f, Just tg' <- getTag g, tg == tg' -> case f $$ x of
			Pure_ y -> appParOpened fs (unsafeCoerce gs) y
			t ::>>= k ->
				(t ::>>= k >< fs, t ::>>= k >< unsafeCoerce gs)
		| otherwise -> (Fun l `app` x, Fun r `app` x)
		{-
		(T, T) -> (Fun fs `app` x, Fun gs `app` x)
		_ -> case f $$ x of
			Pure_ y -> appParOpened tg fs (unsafeCoerce gs) y
			t ::>>= k ->
				(t ::>>= k >< fs, t ::>>= k >< unsafeCoerce gs)
				-}

{-
appParOpened :: (Sequence sq, Funable f, Taggable f) => Id ->
	sq (f (Freer s sq f t)) a b -> sq (f (Freer s sq f t)) a b -> a ->
	(Freer s sq f t b, Freer s sq f t b)
appParOpened tg l r x = case (viewl l, viewl r) of
	(f :<| fs, g :<| gs) -> case (checkClose tg f, checkClose tg g) of
		(T, T) -> (Fun fs `app` x, Fun gs `app` x)
		_ -> case f $$ x of
			Pure_ y -> appParOpened tg fs (unsafeCoerce gs) y
			t ::>>= k ->
				(t ::>>= k >< fs, t ::>>= k >< unsafeCoerce gs)
	_ -> error "never occur: no close tag"
	-}

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
tag (tx ::>>= fs) =
	(tx ::>>=) <$> (\f -> (`putTag` f) <$> Tagged (Id &&& (+ 1))) `mapS` fs
