{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sig where

import Prelude hiding (repeat)

import React

newtype Sig es a b = Sig { unSig :: React es (ISig es a b) }
data ISig es a b = End b | a :| Sig es a b

instance Functor (Sig es a) where
	f `fmap` Sig r = Sig $ (f <$>) <$> r

instance Applicative (Sig es a) where
	pure = emitAll . pure
	Sig mf <*> mx = Sig $ mf >>= \case
		End f -> unSig $ f <$> mx
		h :| mf' -> pure $ h :| ((<$> mx) =<< mf')

instance Monad (Sig es a) where
	Sig l >>= f = Sig $ l >>= \case
		End a -> unSig $ f a
		h :| t -> pure $ h :| (f =<< t)

instance Functor (ISig es a) where
	f `fmap` End b = End $ f b
	f `fmap` (x :| s) = x :| (f <$> s)

instance Applicative (ISig es a) where
	pure = End
	End f <*> mx = f <$> mx
	h :| mf <*> mx = h :| (emitAll . (<$> mx) =<< mf)

instance Monad (ISig es a) where
	End x >>= f = f x
	h :| t >>= f = h :| (emitAll . f =<< t)

type SigG a b = Sig GuiEv a b

cycleColor :: SigG Color Int
cycleColor = cc colors 1 where
	cc :: [Color] -> Int -> SigG Color Int
	cc (h : t) i = do
		emit h
		r <- waitFor . adjust $ middleClick `before` rightClick
		if r then cc t (i + 1) else pure i
	cc [] _ = error "never occur"

colors :: [Color]
colors = cycle [Red .. Magenta]

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

emitAll :: ISig es a b -> Sig es a b
emitAll = Sig . Done

emit :: a -> Sig es a ()
emit a = emitAll (a :| pure ())

waitFor :: React es b -> Sig es a b
waitFor = Sig . (End <$>)

interpretSig :: Monad m =>
	(EvReqs es -> m [EvOcc es]) -> (a -> m ()) -> Sig es a b -> m b
interpretSig p d = interpretSig' where
	interpretSig' (Sig s) = interpret p s >>= interpretISig
	interpretISig (h :| t) = d h >> interpretSig' t
	interpretISig (End a) = pure a

mousePos :: SigG Point ()
mousePos = repeat $ adjust mouseMove

repeat :: React es a -> Sig es a ()
repeat x = xs where xs = Sig $ (:| xs) <$> x
