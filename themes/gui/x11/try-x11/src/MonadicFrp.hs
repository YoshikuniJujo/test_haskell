{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp (
	mouseDown, mouseUp, mouseMove, deltaTime, sleep,
	Point, MouseBtn(..), Time,
	sameClick, clickOn, leftClick, middleClick, rightClick,
	before, doubler, cycleColor, mousePos,
	Rect(..), curRect, elapsed, wiggleRect, posInside,
	firstPoint, completeRect, defineRect,
	Box(..), chooseBoxColor, drClickOn, box, newBoxes, boxes,

	React, first, done,
	Sig, waitFor, emit, repeat, map, scanl, find, at, until, always, (<^>), indexBy, spawn, parList,

	EvReqs, EvOccs, GUIEv(..), Color(..), Event(..),
	interpretSig ) where

import Prelude hiding (map, repeat, scanl, break, until, tail)

import Data.Bool
import Data.Maybe
import Data.Set hiding (map, filter, foldl)

type Reactg = React GUIEv
type Sigg = Sig GUIEv
type ISigg = ISig GUIEv

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

releaseOf :: MouseBtn -> Reactg ()
releaseOf b = mouseUp >>= bool (releaseOf b) (pure ()) . (b `elem`)

leftUp :: Reactg ()
leftUp = releaseOf MLeft

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

posInside :: Rect -> Sigg Point y -> Reactg (Maybe Point)
posInside r = find (`inside` r)

inside :: Point -> Rect -> Bool
inside (x, y) (Rect (l, u) (r, d)) =
	(l <= x && x <= r || r <= x && x <= l) &&
	(u <= y && y <= d || d <= y && y <= u)

firstPoint :: Reactg Point
firstPoint = maybe firstPoint pure =<< mousePos `at` leftClick

completeRect :: Point -> Sigg Rect (Maybe Rect)
completeRect p1 = cur . fst <$> curRect p1 `until` leftUp

defineRect :: Sigg Rect Rect
defineRect = waitFor firstPoint >>= \p1 ->
	completeRect p1 >>= \case
		Just r -> pure r
		Nothing -> defineRect

chooseBoxColor :: Rect -> Sigg Box ()
chooseBoxColor r = () <$ always Box <^> wiggleRect r <^> cycleColor

drClickOn :: Rect -> Reactg (Maybe Point)
drClickOn r = posInside r $ mousePos `indexBy` repeat doubler

box :: Sigg Box ()
box = do
	r <- setColor `map` defineRect
	chooseBoxColor r
	() <$ waitFor (drClickOn r)
	where
	setColor r = Box r (head colors)

newBoxes :: Sigg (ISigg Box ()) ()
newBoxes = spawn box

boxes :: Sigg [Box] ()
boxes = parList newBoxes

data Box = Box Rect Color deriving Show

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

instance Functor (ISig e a) where
	f `fmap` End x = End $ f x
	f `fmap` (h :| t) = h :| (f <$> t)

instance Applicative (ISig e a) where
	pure = End
	mf <*> mx = (<$> mx) =<< mf

instance Monad (ISig e a) where
	End a >>= f = f a
	(h :| t) >>= f = h :| (emitAll . f =<< t)

waitFor :: React e a -> Sig e x a
waitFor = Sig . (End <$>)

always :: a -> Sig e a b
always a = emit a >> hold

hold :: Sig e x a
hold = waitFor never where never = Await empty undefined

emit :: a -> Sig e a ()
emit a = emitAll (a :| pure ())

emitAll :: ISig e a b -> Sig e a b
emitAll = Sig . Done

repeat :: React e a -> Sig e a ()
repeat x = xs where xs = Sig $ (:| xs) <$> x

spawn :: Sig e a b -> Sig e (ISig e a b) ()
spawn (Sig l) = repeat l

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

break :: (a -> Bool) -> Sig e a b -> Sig e a (ISig e a b)
break f (Sig l) = Sig (ibreak f <$> l)

ibreak :: (a -> Bool) -> ISig e a b -> ISig e a (ISig e a b)
ibreak f (h :| t)
	| f h = pure $ h :| t
	| otherwise = h :| break f t
ibreak _ (End a) = pure $ End a

res :: Sig e a b -> React e b
res (Sig l) = l >>= ires

ires :: ISig e a b -> React e b
ires (_ :| t) = res t
ires (End a) = Done a

cur :: Sig e a b -> Maybe a
cur (Sig (Done (h :| _))) = Just h
cur _ = Nothing

icur :: ISig e a b -> Maybe a
icur (h :| _) = Just h
icur (End _) = Nothing

find :: (a -> Bool) -> Sig e a b -> React e (Maybe a)
find f l = icur <$> res (break f l)

at :: Ord e => Sig e a c -> React e b -> React e (Maybe a)
l `at` a = cur . fst <$> res (l `until` a)

until :: Ord e => Sig e x b -> React e a -> Sig e x (Sig e x b, React e a)
Sig l `until` a = waitFor (first l a) >>= un where
	un (Done l', a') = do
		(l'', a'') <- emitAll (l' `iuntil` a')
		pure (emitAll l'', a'')
	un (l', a') = pure (Sig l', a')

iuntil :: Ord e => ISig e a c -> React e b -> ISig e a (ISig e a c, React e b)
End l `iuntil` a = End (End l, a)
(h :| Sig t) `iuntil` a = h :| Sig (cont <$> first t a)
	where
	cont (Done l', a') = l' `iuntil` a'
	cont (t', Done a') = End (h :| Sig t', Done a')
	cont (Await _ _, Await _ _) = error "never occur"

(<^>) :: Ord e => Sig e (t -> x) b1 -> Sig e t b2 -> Sig e x (ISig e (t -> x) b1, ISig e t b2)
l <^> r = do
	(l', r') <- waitFor $ bothStart l r
	emitAll $ imap (\(f, a) -> f a) (pairs l' r')

bothStart :: Ord e => Sig e a1 b1 -> Sig e a2 b2 -> React e (ISig e a1 b1, ISig e a2 b2)
bothStart l (Sig r) = do
	(Sig l', r') <- res $ l `until` r
	(Sig r'', l'') <- res $ Sig r' `until` l'
	pure (done' l'', done' r'')

indexBy :: Ord e => Sig e a l -> Sig e b r -> Sig e a ()
l `indexBy` (Sig r) = do
	(Sig l', r') <- waitFor . res $ l `until` r
	case (l', r') of
		(_, Done (End _)) -> pure ()
		(Done l'', r'') -> l'' `iindexBy` Sig r''
		(l'', Done (_ :| r'')) -> Sig l'' `indexBy` r''
		(Await _ _, Await _ _) -> error "never occur"

iindexBy :: Ord e => ISig e x b1 -> Sig e a b2 -> Sig e x ()
l `iindexBy` (Sig r) = do
	(l', r') <- waitFor . ires $ l `iuntil` r
	case (l', r') of
		(hl :| tl, Done (_hr :| tr)) -> emit hl >> (hl :| tl) `iindexBy` tr
		_ -> pure ()

done' ::  React e c -> c
done' = fromJust . done

cons :: Ord e => ISig e a l -> ISig e [a] r -> ISig e [a] ()
cons h t = do
	(h', t') <- imap (uncurry (:)) (pairs h t)
	_ <- imap (: []) h'
	() <$ t'

parList :: Ord e => Sig e (ISig e a l) r -> Sig e [a] ()
parList = emitAll . iparList

iparList :: Ord e => Sig e (ISig e a l) r -> ISig e [a] ()
iparList l = rl ([] :| hold) l >> pure () where
	rl t (Sig es) = do
		(t', es') <- t `iuntil` es
		case es' of
			Done (e :| es'') -> rl (cons e t') es''
			_ -> t'

pairs :: Ord e => ISig e a1 b1 -> ISig e a2 b2 -> ISig e (a1, a2) (ISig e a1 b1, ISig e a2 b2)
pairs a@(End _) b = End (a, b)
pairs a b@(End _) = End (a, b)
pairs (hl :| Sig tl) (hr :| Sig tr) = (hl, hr) :| tail
	where
	tail = Sig $ cont <$> first tl tr
	cont (tl', tr') = pairs (lup hl tl') (lup hr tr')
	lup _ (Done l) = l
	lup h t = h :| Sig t
