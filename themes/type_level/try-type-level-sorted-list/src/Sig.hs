{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sig where

import Prelude hiding (map, repeat, scanl)

import Data.Time

import React

infixr 5 :|
newtype Sig es a r = Sig { unSig :: React es (ISig es a r) }
data ISig es a r = End r | a :| Sig es a r

type SigG = Sig GuiEv
type ISigG = ISig GuiEv

instance Functor (Sig es a) where
	f `fmap` Sig s = Sig $ (f <$>) <$> s

instance Applicative (Sig es a) where
	pure = Sig . pure . pure
	Sig lf <*> mx = Sig (lf >>= ib) where
		ib (End f) = unSig $ f <$> mx
		ib (h :| t) = pure $ h :| (t >>= (<$> mx))

instance Monad (Sig es a) where
	Sig l >>= f = Sig (l >>= ib) where
		ib (End x) = unSig $ f x
		ib (h :| t) = pure $ h :| (t >>= f)

instance Functor (ISig es a) where
	f `fmap` End x = End $ f x
	f `fmap` (h :| t) = h :| (f <$> t)

instance Applicative (ISig es a) where
	pure = End
	End f <*> mx = f <$> mx
	(h :| tf) <*> mx = h :| (tf >>= Sig . pure . (<$> mx))

instance Monad (ISig es a) where
	End r >>= f = f r
	(h :| t) >>= f = h :| (t >>= Sig . pure . f)

emitAll :: ISig es a b -> Sig es a b
emitAll = Sig . pure

emit :: a -> Sig es a ()
emit a = emitAll $ a :| pure ()

waitFor :: React es r -> Sig es a r
waitFor = Sig . (pure <$>)

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

interpretSig :: Monad m =>
	(EvReqs es -> m (EvOccs es)) -> (a -> m ()) -> Sig es a r -> m r
interpretSig p d = interpretSig' where
	interpretSig' (Sig s) = interpret p s >>= interpretISig
	interpretISig (End x) = pure x
	interpretISig (h :| t) = d h >> interpretSig' t

repeat :: React es a -> Sig es a ()
repeat x = xs where xs = Sig $ (:| xs) <$> x

mousePos :: SigG Point ()
mousePos = repeat $ adjust mouseMove

map :: (a -> b) -> Sig es a r -> Sig es b r
map f (Sig l) = Sig $ (f `imap`) <$> l

imap :: (a -> b) -> ISig es a r -> ISig es b r
imap f (h :| t) = f h :| (f `map` t)
imap _ (End x) = End x

curRect :: Point -> SigG Rect ()
curRect p1 = Rect p1 `map` mousePos

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show

scanl :: (a -> b -> a) -> a -> Sig es b r -> Sig es a r
scanl f i = emitAll . iscanl f i

iscanl :: (a -> b -> a) -> a -> Sig es b r -> ISig es a r
iscanl f i (Sig l) = i :| (waitFor l >>= lsl) where
	lsl (h :| t) = scanl f (f i h) t
	lsl (End x) = pure x

elapsed :: SigG DiffTime ()
elapsed = scanl (+) 0 . repeat $ adjust deltaTime
