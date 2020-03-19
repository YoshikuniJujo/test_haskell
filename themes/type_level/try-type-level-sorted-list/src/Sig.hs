{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sig where

import Prelude hiding (map, repeat, scanl, break, until)

import Data.Time

import React
import OpenUnionValue
import Sorted

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

wiggleRect :: Rect -> SigG Rect ()
wiggleRect (Rect lu rd) = rectAtTime `map` elapsed where
	rectAtTime t = Rect (lu +. dx) (rd +. dx) where
		dx = (round (sin (fromRational (toRational t) * 5) * 15 :: Double), 0)

(+.) :: Point -> Point -> Point
(x1, y1) +. (x2, y2) = (x1 + x2, y1 + y2)

find :: (a -> Bool) -> Sig es a r -> React es (Either a r)
find p l = icur <$> res (break p l)

cur :: Sig es a b -> Maybe a
cur (Sig (Done (h :| _))) = Just h
cur _ = Nothing

icur :: ISig es a b -> Either a b
icur (h :| _) = Left h
icur (End r) = Right r

res :: Sig es a b -> React es b
res (Sig l) = ires =<< l

ires :: ISig es a b -> React es b
ires (_ :| t) = res t
ires (End a) = pure a

break :: (a -> Bool) -> Sig es a r -> Sig es a (ISig es a r)
break p (Sig l) = Sig $ ibreak p <$> l

ibreak :: (a -> Bool) -> ISig es a r -> ISig es a (ISig es a r)
ibreak p is@(h :| t)
	| p h = pure is
	| otherwise = h :| break p t
ibreak _ is@(End _) = pure is

posInside :: Rect -> SigG Point y -> ReactG (Either Point y)
posInside r = find (`inside` r)

inside :: Point -> Rect -> Bool
inside (x, y) (Rect (l, u) (r, d)) =
	(l <= x && x <= r || r <= x && x <= l) &&
	(u <= y && y <= d || d <= y && y <= u)

at :: (	Merge es' es ~ Merge es es',
	Convert es (Merge es' es), Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es') ) =>
	Sig es a b1 -> React es' b2 -> React (Merge es es') (Maybe a)
l `at` a = cur . fst <$> res (l `until` a)

until :: (
	Merge es es' ~ Merge es' es,
	Convert es (Merge es' es), Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es') ) =>
	Sig es a r -> React es' b -> Sig (Merge es' es) a (Sig es a r, React es' b)
until (Sig l) a = waitFor (l `first` a) >>= un where
	un (Done l', a') = do
		(l'', a'') <- emitAll $ l' `iuntil` a'
		pure (emitAll l'', a'')
	un (l', a') = pure (Sig l', a')

iuntil :: (
	Merge es es' ~ Merge es' es,
	Convert es (Merge es' es), Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es') ) =>
	ISig es a r -> React es' b -> ISig (Merge es es') a (ISig es a r, React es' b)
iuntil (End l) a = End (End l, a)
iuntil (h :| Sig t) a = h :| Sig (cont <$> t `first` a)
	where
	cont (Done l', a') = l' `iuntil` a'
	cont (t', Done a') = End (h :| Sig t', Done a')
	cont _ = error "never occur"

firstPoint :: ReactG (Maybe Point)
firstPoint = mousePos `at` leftClick

completeRect :: Point -> SigG Rect (Maybe Rect)
completeRect p1 = cur . fst <$> curRect p1 `until` leftUp

defineRect :: SigG Rect Rect
defineRect = waitFor firstPoint >>= \case
	Just p1 -> completeRect p1 >>= \case
		Just r -> pure r
		Nothing -> error "bad"
	Nothing -> error "bad"
