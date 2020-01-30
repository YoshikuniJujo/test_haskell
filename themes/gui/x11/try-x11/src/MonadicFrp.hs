-- {-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp (
	mouseDown, mouseUp, mouseMove,

	EvReqs, EvOccs, GUIEv(..), Color(..), MouseBtn(..), Event(..),
	interpretSig, cycleColor ) where

import Data.Set hiding (map, filter, foldl)

mouseDown, mouseUp :: Reactg [MouseBtn]
mouseDown = exper (MouseDown Request) >>= get
	where
	get (MouseDown (Occured s)) = pure s
	get _ = error "bad"

mouseUp = exper (MouseUp Request) >>= get
	where
	get (MouseUp (Occured s)) = pure s
	get _ = error "bad"

mouseMove :: Reactg Point
mouseMove = exper (MouseMove Request) >>= get
	where
	get (MouseMove (Occured p)) = pure p
	get _ = error "bad"

exper :: e -> React e e
exper a = Await (singleton a) (Done . head . elems)

type Point = (Double, Double)
data MouseBtn = MLeft | MMiddle | MRight deriving (Eq, Show, Ord)
type Time = Double

type EvReqs e = Set e
type EvOccs e = Set e

data React e a
	= Done a
	| Await (EvReqs e) (EvOccs e -> React e a)

data Event a = Request | Occured a deriving Show

instance Ord a => Ord (Event a) where
	Occured a `compare` Occured b = a `compare` b
	_ `compare` _ = EQ

instance Ord a => Eq (Event a) where
	a == b = a `compare` b == EQ

data GUIEv
	= MouseDown (Event [MouseBtn])
	| MouseUp (Event [MouseBtn])
	| MouseMove (Event Point)
	| DeltaTime (Event Time)
	| TryWait Time (Event Time)
	deriving (Eq, Show, Ord)

type Reactg = React GUIEv

instance Functor (React e) where
	f `fmap` Done v = Done $ f v
	f `fmap` Await e c = Await e $ (Done . f =<<) . c

instance Applicative (React e) where
	pure = Done
	Done f <*> mx = f <$> mx
	Await e k <*> mx = Await e $ ((<$> mx) =<<) . k

instance Monad (React e) where
	Done v >>= f = f v
	Await e c >>= f = Await e (\x -> c x >>= f)

newtype Sig e a b = Sig (React e (ISig e a b))
data ISig e a b = a :| Sig e a b | End b

type Sigg = Sig GUIEv
-- type ISigg = ISig GUIEv

interpret :: Monad m => (EvReqs e -> m (EvOccs e)) -> React e a -> m a
interpret _ (Done a) = pure a
interpret p (Await e c) = p e >>= interpret p . c

interpretSig :: Monad m =>
	(EvReqs e -> m (EvOccs e)) -> (a -> m r) -> Sig e a b -> m b
interpretSig p d (Sig s) = interpret p s >>= \case
	h :| t -> d h >> interpretSig p d t
	End a -> pure a

cycleColor :: Sigg Color Int
cycleColor = cc colors 1 where
	cc [] _ = error "never occur"
	cc (h : t) i = do
		emit h
		r <- waitFor (middleClick `before` rightClick)
		if r then cc t (i + 1) else return i

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

colors :: [Color]
colors = cycle [Red .. Magenta]

emitAll :: ISig e a b -> Sig e a b
emitAll = Sig . Done

emit :: a -> Sig e a ()
emit a = emitAll (a :| pure ())

instance Functor (Sig e a) where
	f `fmap` m = pure . f =<< m

instance Applicative (Sig e a) where
	pure = emitAll . End
	mf <*> mx = (<$> mx) =<< mf

instance Monad (Sig e a) where
	Sig l >>= f = Sig (l >>= ib)
		where
		ib (h :| t) = pure (h :| (t >>= f))
		ib (End a) = let Sig x = f a in x

waitFor :: React e a -> Sig e x a
waitFor a = Sig $ End <$> a

first :: Ord e => React e a -> React e b -> React e (React e a, React e b)
first l r = case (l, r) of
	(Await el _, Await er _) -> let
		e = el `union` er
		c b = first (update l b) (update r b) in
		Await e c
	_ -> Done (l, r)

update :: Ord e => React e a -> EvOccs e -> React e a
update (Await e c) b
	| b' /= empty = c b'
	where b' = b `filterOccs` e
update r _ = r

filterOccs :: Ord e => EvOccs e -> EvReqs e -> EvOccs e
filterOccs = intersection

done :: React e a -> Maybe a
done (Done a) = Just a
done _ = Nothing

before :: Reactg a -> Reactg b -> Reactg Bool
before a b = do
	(a', b') <- first a b
	case (done a', done b') of
		(Just _, Nothing) -> pure True
		_ -> pure False

clickOn :: MouseBtn -> Reactg ()
clickOn b = do
	bs <- mouseDown
	if b `elem` bs then return () else clickOn b

middleClick, rightClick :: Reactg ()
middleClick = clickOn MMiddle
rightClick = clickOn MRight
