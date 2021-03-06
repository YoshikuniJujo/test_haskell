{-# LANGUAGE LambdaCase, TypeFamilies, FlexibleContexts, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Sig where

import Prelude hiding (map, repeat, scanl, break, until, tail)

import Control.Monad
import Data.Maybe
import Data.Time

import qualified Control.Arrow as A

import Sorted
import OpenUnionValue
import Old.React

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
type ISigG a b = ISig GuiEv a b

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
interpretSig p d = interpretSig' [] where
	interpretSig' occs (Sig s) = do
		(a', occs') <- interpret (\x -> (occs ++) <$> p x) s
		interpretISig occs' a'
	interpretISig occs' (h :| t) = d h >> interpretSig' occs' t
	interpretISig _ (End a) = pure a
	{-
	runAllSig occs (Sig s) = runAllISig occs s
	runAllISig occ
	-}

{-
interpretSig p d s@(Await reqs _) = p reqs >>= \occs -> do
	let	(xs, sr) = s `applySig` occs
	d `mapM_` xs
	case sr of
		Left s' -> interpretSig p d s'
		Right r -> pure r
		-}

applySig :: Sig es a r -> [EvOcc es] -> ([a], Either (Sig es a r) r)
Sig s `applySig` occs = case s `apply` occs of
	Left s' -> ([], Left $ Sig s')
	Right (is, occs') -> is `applyISig` occs'

applyISig :: ISig es a r -> [EvOcc es] -> ([a], Either (Sig es a r) r)
End r `applyISig` _ = ([], Right r)
(h :| t) `applyISig` [] = ([h], Left t)
(h :| t) `applyISig` occs = (h :) `A.first` (t `applySig` occs)

mousePos :: SigG Point ()
mousePos = repeat $ adjust mouseMove

repeat :: React es a -> Sig es a ()
repeat x = xs where xs = Sig $ (:| xs) <$> x

map :: (a -> b) -> Sig es a r -> Sig es b r
map f (Sig l) = Sig $ (f `imap`) <$> l

imap :: (a -> b) -> ISig es a r -> ISig es b r
imap f (h :| t) = f h :| (f `map` t)
imap _ (End x) = End x

curRect :: Point -> SigG Rect ()
curRect p1 = Rect p1 `map` mousePos

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show

scanl :: (a -> b -> a) -> a -> Sig es b r -> Sig es a r
scanl f i l = emitAll $ iscanl f i l

iscanl :: (a -> b -> a) -> a -> Sig es b r -> ISig es a r
iscanl f i (Sig l) = i :| (waitFor l >>= lsl)
	where
	lsl (h :| t) = scanl f (f i h) t
	lsl (End a) = pure a

elapsed :: SigG DiffTime ()
elapsed = scanl (+) 0 . repeat $ adjust deltaTime

wiggleRect :: Rect -> SigG Rect ()
wiggleRect (Rect lu rd) = rectAtTime `map` elapsed
	where
	rectAtTime t = Rect (lu +. dx) (rd +. dx)
		where dx = (round (sin (fromRational . toRational $ t * 5) * 15 :: Double), 0)

(+.) :: Point -> Point -> Point
(x1, y1) +. (x2, y2) = (x1 + x2, y1 + y2)

find :: (a -> Bool) -> Sig es a r -> React es (Either a r)
find f l = icur <$> res (break f l)

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
ires (End a) = Done a

break :: (a -> Bool) -> Sig es a b -> Sig es a (ISig es a b)
break f (Sig l) = Sig $ ibreak f <$> l

ibreak :: (a -> Bool) -> ISig es a b -> ISig es a (ISig es a b)
ibreak f is@(h :| t)
	| f h = pure is
	| otherwise = h :| break f t
ibreak _ is@(End _) = pure is

posInside :: Rect -> SigG Point y -> ReactG (Either Point y)
posInside r = find (`inside` r)

inside :: Point -> Rect -> Bool
inside (x, y) (Rect (l, u) (r, d)) =
	(l <= x && x <= r || r <= x && x <= l) &&
	(u <= y && y <= d || d <= y && y <= u)

at :: (
	Merge es es' ~ Merge es' es,
	Convert es' (Merge es' es),
	Convert es (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es'),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es)
	) => Sig es a y -> React es' b -> React (Merge es es') (Maybe a)
l `at` a = cur . fst <$> res (l `until` a)

until :: (
	Convert es' (Merge es' es), Convert es (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es'),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Merge es es' ~ Merge es' es) =>
	Sig es a r -> React es' b -> Sig (Merge es es') a (Sig es a r, React es' b)
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
completeRect p1 = do
	(r, _) <- curRect p1 `until` leftUp
	pure $ cur r

defineRect :: SigG Rect Rect
defineRect = waitFor firstPoint >>= \case
	Just p1 -> completeRect p1 >>= \case
		Just r -> pure r
		Nothing -> error "completeRect: bad"
	Nothing -> error "firstPoint: bad"

always :: (Convert es es, Convert (Map Occurred es) (Map Occurred es)) => a -> Sig es a r
always a = emit a >> hold

hold :: (Convert es es, Convert (Map Occurred es) (Map Occurred es)) => Sig es a r
hold = waitFor $ adjust never

(<^>) :: (
	Convert es1 (Merge es2 es1),
	Convert es2 (Merge es2 es1),
	Convert (Map Occurred (Merge es2 es1)) (Map Occurred es1),
	Convert (Map Occurred (Merge es2 es1)) (Map Occurred es2),
	Merge es1 es2 ~ Merge es2 es1 ) =>
	Sig es2 (a1 -> a2) b1 -> Sig es1 a1 b3 -> Sig (Merge es2 es1) a2 (ISig es2 (a1 -> a2) b1, ISig es1 a1 b3)
l <^> r = do
	(l', r') <- waitFor $ bothStart l r
	emitAll $ uncurry ($) `imap` pairs l' r'

bothStart :: (
	Convert es2 (Merge es1 es2),
	Convert es1 (Merge es1 es2),
	Convert (Map Occurred (Merge es1 es2)) (Map Occurred es2),
	Convert (Map Occurred (Merge es1 es2)) (Map Occurred es1),
	Merge es2 es1 ~ Merge es1 es2
	) => Sig es1 a1 r1 -> Sig es2 a2 r2 -> React (Merge es1 es2) (ISig es1 a1 r1, ISig es2 a2 r2)
bothStart l (Sig r) = do
	(Sig l', r') <- res $ l `until` r
	(Sig r'', l'') <- res (Sig r' `until` l')
	pure (done' l'', done' r'')

done' :: React es a -> a
done' = fromJust . done

pairs :: (
	Convert es2 (Merge es1 es2),
	Convert es1 (Merge es1 es2),
	Convert (Map Occurred (Merge es1 es2)) (Map Occurred es2),
	Convert (Map Occurred (Merge es1 es2)) (Map Occurred es1),
	Merge es2 es1 ~ Merge es1 es2 ) =>
	ISig es2 a b1 -> ISig es1 b2 b3 -> ISig (Merge es1 es2) (a, b2) (ISig es2 a b1, ISig es1 b2 b3)
pairs (End a) b = End (End a, b)
pairs a (End b) = End (a, End b)
pairs (hl :| Sig tl) (hr :| Sig tr) = (hl, hr) :| tail
	where
	tail = Sig $ cont <$> (tl `first` tr)
	cont (tl', tr') = pairs (lup hl tl') (lup hr tr')
	lup _ (Done l) = l
	lup h t = h :| Sig t

chooseBoxColor :: Rect -> SigG Box ()
chooseBoxColor r = () <$ (always Box :: SigG (Rect -> Color -> Box) ()) <^> wiggleRect r <^> cycleColor

data Box = Box Rect Color deriving Show

drClickOn :: Rect -> ReactG (Either Point ())
drClickOn r = posInside r $ mousePos `indexBy` repeat doubler

indexBy :: (
	Convert es (Merge es' es),
	Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es'),
	Merge es es' ~ Merge es' es
	) => Sig es a b -> Sig es' a2 b2 -> Sig (Merge es' es) a ()
l `indexBy` Sig r = do
	(Sig l', r') <- waitFor . res $ l `until` r
	case (l', r') of
		(_, Done (End _)) -> pure ()
		(Done l'', r'') -> l'' `iindexBy` Sig r''
		(l'', Done (_ :| r'')) -> Sig l'' `indexBy` r''
		_ -> error "never occur"

iindexBy :: (
	Convert es (Merge es' es),
	Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es'),
	Merge es es' ~ Merge es' es) =>
	ISig es a1 b1 -> Sig es' a2 b2 -> Sig (Merge es' es) a1 ()
l `iindexBy` Sig r = do
	(l', r') <- waitFor . ires $ l `iuntil` r
	case (l', r') of
		(hl :| tl, Done (_hr :| tr)) -> emit hl >> (hl :| tl) `iindexBy` tr
		_ -> pure ()

box :: SigG Box ()
box = () <$ do
	r <- (`Box` Red) `map` defineRect
	chooseBoxColor r
	waitFor $ drClickOn r

spawn :: Sig es a r -> Sig es (ISig es a r) ()
spawn (Sig l) = repeat l

parList :: (
	Convert es es,
	Convert es' es,
	Convert (Map Occurred es) (Map Occurred es'),
	Convert (Map Occurred es) (Map Occurred es),
	Merge es es' ~ Merge es' es,
	Merge es es ~ es,
	Merge es' es ~ es
	) =>
	Sig es' (ISig es a1 a2) b -> Sig es [a1] ()
parList x = emitAll $ iparList x

iparList :: (
	Convert es' es, Convert es es,
	Convert (Map Occurred es) (Map Occurred es'),
	Convert (Map Occurred es) (Map Occurred es),
	Merge es es ~ es,
	Merge es' es ~ es,
	Merge es es' ~ Merge es' es ) =>
	Sig es' (ISig es a1 a2) b -> ISig es [a1] ()
iparList l = rl ([] :| hold) l >> pure () where
	rl t (Sig es) = do
		(t', es') <- t `iuntil` es
		case es' of
			Done (e'' :| es'') -> rl (cons e'' t') es''
			_ -> t'

cons :: (
	Convert es es,
	Convert (Map Occurred es) (Map Occurred es),
	Merge es es ~ es ) =>
	ISig es a1 a2 -> ISig es [a1] a3 -> ISig es [a1] ()
cons h t = () <$ do
	(h', t') <- uncurry (:) `imap` pairs h t
	void $ (: []) `imap` h'
	void t'

newBoxes :: SigG (ISigG Box ()) ()
newBoxes = spawn box

boxes :: SigG [Box] ()
boxes = parList newBoxes
