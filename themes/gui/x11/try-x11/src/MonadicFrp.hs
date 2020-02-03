{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp (
	mouseDown, mouseUp, mouseMove, deltaTime, sleep,
	Point, MouseBtn(..), Time,
	sameClick, clickOn, leftClick, middleClick, rightClick,
	before, doubler, cycleColor, mousePos,
	Rect(..), curRect, elapsed, wiggleRect,

	React, first, done,
	Sig, waitFor, emit, repeat, map, scanl,

	EvReqs, EvOccs, GUIEv(..), Color(..), Event(..),
	interpretSig ) where

import Prelude hiding (map, repeat, scanl)

import Data.Bool
import Data.Set hiding (map, filter, foldl)

type Reactg = React GUIEv
type Sigg = Sig GUIEv

data GUIEv
	= MouseDown (Event [MouseBtn])
	| MouseUp (Event [MouseBtn])
	| MouseMove (Event Point)
	| DeltaTime (Event Time)
	| TryWait Time (Event Time)
	deriving (Eq, Show, Ord)

data Event a = Request | Occurred a deriving Show

instance Ord a => Ord (Event a) where
	Occurred a `compare` Occurred b = a `compare` b
	_ `compare` _ = EQ

instance Ord a => Eq (Event a) where
	a == b = a `compare` b == EQ

mouseDown, mouseUp :: Reactg [MouseBtn]
mouseDown = exper (MouseDown Request) >>= \case
	MouseDown (Occurred s) -> pure s; _ -> error "bad"

mouseUp = exper (MouseUp Request) >>= \case
	MouseUp (Occurred s) -> pure s; _ -> error "bad"

mouseMove :: Reactg Point
mouseMove = exper (MouseMove Request) >>= \case
	MouseMove (Occurred p) -> pure p; _ -> error "bad"

deltaTime :: Reactg Time
deltaTime = exper (DeltaTime Request) >>= \case
	DeltaTime (Occurred t) -> pure t; _ -> error "bad"

sleep :: Time -> Reactg ()
sleep t = tryWait t >>= \t' -> if t' == t then pure () else sleep (t - t')

tryWait :: Time -> Reactg Time
tryWait t = exper (TryWait t Request) >>= \case
	TryWait _ (Occurred t') -> pure t'; _ -> error "bad"

type Point = (Double, Double)
data MouseBtn = MLeft | MMiddle | MRight deriving (Show, Eq, Ord)
type Time = Double

sameClick :: Reactg Bool
sameClick = (==) <$> mouseDown <*> mouseDown

clickOn :: MouseBtn -> Reactg ()
clickOn b = mouseDown >>= bool (clickOn b) (pure ()) . (b `elem`)

leftClick, middleClick, rightClick :: Reactg ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

before :: Reactg a -> Reactg b -> Reactg Bool
before a b = first a b >>= \(a', b') -> case (done a', done b') of
	(Just _, Nothing) -> pure True
	_ -> pure False

doubler :: Reactg ()
doubler = do
	rightClick
	bool doubler (pure ()) =<< rightClick `before` sleep 0.2

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

cycleColor :: Sigg Color Int
cycleColor = cc colors 1 where
	cc [] _ = error "never occur"
	cc (c : cs) i = do
		emit c
		bool (return i) (cc cs $ i + 1)
			=<< waitFor (middleClick `before` rightClick)

colors :: [Color]
colors = cycle [Red .. Magenta]

mousePos :: Sigg Point ()
mousePos = repeat mouseMove

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show

curRect :: Point -> Sigg Rect ()
curRect p1 = Rect p1 `map` mousePos

elapsed :: Sigg Time ()
elapsed = scanl (+) 0 $ repeat deltaTime

wiggleRect :: Rect -> Sigg Rect ()
wiggleRect (Rect lu rd) = rectAtTime `map` elapsed
	where
	rectAtTime t = Rect (lu +. dx) (rd +. dx)
		where dx = (sin (t * 5) * 15, 0)

(+.) :: Num n => (n, n) -> (n, n) -> (n, n)
(x1, y1) +. (x2, y2) = (x1 + x2, y1 + y2)

data React e a = Done a | Await (EvReqs e) (EvOccs e -> React e a)
type EvReqs e = Set e
type EvOccs e = Set e

interpret :: Monad m => (EvReqs e -> m (EvOccs e)) -> React e a -> m a
interpret _ (Done a) = pure a
interpret p (Await e c) = p e >>= interpret p . c

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

exper :: e -> React e e
exper a = Await (singleton a) (Done . head . elems)

first :: Ord e => React e a -> React e b -> React e (React e a, React e b)
first = curry \case
	(l@(Await el _), r@(Await er _)) ->
		Await (el `union` er) $ first <$> update l <*> update r
	lr -> Done lr

update :: Ord e => React e a -> EvOccs e -> React e a
update (Await e c) b | b' /= empty = c b' where b' = b `filterOccs` e
update r _ = r

filterOccs :: Ord e => EvOccs e -> EvReqs e -> EvOccs e
filterOccs = intersection

done :: React e a -> Maybe a
done = \case Done a -> Just a; _ -> Nothing

newtype Sig e a b = Sig (React e (ISig e a b))
data ISig e a b = a :| Sig e a b | End b

interpretSig :: Monad m =>
	(EvReqs e -> m (EvOccs e)) -> (a -> m r) -> Sig e a b -> m b
interpretSig p d (Sig s) = interpret p s >>= \case
	h :| t -> d h >> interpretSig p d t
	End a -> pure a

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
waitFor = Sig . (End <$>)

emit :: a -> Sig e a ()
emit a = emitAll (a :| pure ())

emitAll :: ISig e a b -> Sig e a b
emitAll = Sig . Done

repeat :: React e a -> Sig e a ()
repeat x = xs where xs = Sig $ (:| xs) <$> x

map :: (a -> b) -> Sig e a c  -> Sig e b c
f `map` Sig rct = Sig $ (f `imap`) <$> rct

imap :: (a -> b) -> ISig e a c -> ISig e b c
_ `imap` End x = End x
f `imap` (h :| t) = f h :| (f `map` t)

scanl :: (b -> a -> b) -> b -> Sig e a c -> Sig e b c
scanl f i (Sig l) = Sig $ iscanl f i <$> l

iscanl :: (b -> a -> b) -> b -> ISig e a c -> ISig e b c
iscanl _ _ (End x) = End x
iscanl f i (h :| t) = i :| scanl f (f i h) t
