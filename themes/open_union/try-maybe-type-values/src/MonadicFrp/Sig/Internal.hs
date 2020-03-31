{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGe FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.Sig.Internal (
	-- * Types
	Sig, ISig(..),
	-- * Run Sig
	interpretSig,
	-- * Conversion
	cur, emit, always, waitFor,
	-- * Transformation
	map, scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel composition
	at, until, (<^>), indexBy
	) where

import Prelude hiding (map, repeat, scanl, break, until, tail)
import Control.Monad
import Data.Maybe

import MonadicFrp.React.Internal
import Data.UnionList
import Data.Sorted hiding (Merge)

infixr 5 :|
newtype Sig es a r = Sig { unSig :: React es (ISig es a r) }
data ISig es a r = End r | a :| Sig es a r

instance Functor (Sig es a) where
	f `fmap` Sig s = Sig $ (f <$>) <$> s

instance Applicative (Sig es a) where
	pure = Sig . pure . pure
	Sig lf <*> mx = Sig $ lf >>= ib where
		ib (End f) = unSig $ f <$> mx
		ib (h :| t) = pure $ h :| (t >>= (<$> mx))

instance Monad (Sig es a) where
	Sig l >>= f = Sig $ l >>= ib where
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

interpretSig :: Monad m =>
	(EvReqs es -> m (EvOccs es)) -> (a -> m ()) -> Sig es a r -> m r
interpretSig p d = interpretSig' where
	interpretSig' (Sig s) = interpret p s >>= interpretISig
	interpretISig (End x) = pure x
	interpretISig (h :| t) = d h >> interpretSig' t

emitAll :: ISig es a b -> Sig es a b
emitAll = Sig . pure

emit :: a -> Sig es a ()
emit a = emitAll $ a :| pure ()

waitFor :: React es r -> Sig es a r
waitFor = Sig . (pure <$>)

repeat :: React es a -> Sig es a ()
repeat x = xs where xs = Sig $ (:| xs) <$> x

map :: (a -> b) -> Sig es a r -> Sig es b r
f `map` Sig l = Sig $ (f `imap`) <$> l

imap :: (a -> b) -> ISig es a r -> ISig es b r
f `imap` (h :| t) = f h :| (f `map` t)
_ `imap` (End x) = pure x

scanl :: (b -> a -> b) -> b -> Sig es a r -> Sig es b r
scanl f i = emitAll . iscanl f i

iscanl :: (b -> a -> b) -> b -> Sig es a r -> ISig es b r
iscanl f i (Sig l) = i :| (waitFor l >>= lsl) where
	lsl (h :| t) = scanl f (f i h) t
	lsl (End x) = pure x

find :: (a -> Bool) -> Sig es a r -> React es (Either a r)
find p l = icur <$> res (break p l)

cur :: Sig es a r -> Maybe a
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

at :: Mergeable es es' =>
	Sig es a r -> React es' r' -> React (es :+: es') (Maybe a)
l `at` a = cur . fst <$> res (l `until` a)

until :: Mergeable es es' => Sig es a r ->
	React es' b -> Sig (es :+: es') a (Sig es a r, React es' b)
Sig l `until` a = waitFor (l `first` a) >>= un where
	un (Done l', a') = do
		(l'', a'') <- emitAll $ l' `iuntil` a'
		pure (emitAll l'', a'')
	un (l', a') = pure (Sig l', a')

iuntil :: Mergeable es es' => ISig es a r ->
	React es' b -> ISig (es :+: es') a (ISig es a r, React es' b)
End l `iuntil` a = pure (pure l, a)
(h :| Sig t) `iuntil` a = h :| Sig (cont <$> t `first` a) where
	cont (Done l', a') = l' `iuntil` a'
	cont (t', Done a') = pure (h :| Sig t', pure a')
	cont _ = error "never occur"

hold :: (Nihil es, Mergeable 'Nil es) => Sig es a r
hold = waitFor $ adjust never

always :: (
	Nihil es, Merge 'Nil es es,
	CollapseOccurred 'Nil es ) => a -> Sig es a r
always a = emit a >> hold

(<^>) :: (Mergeable es es', Mergeable es' es) =>
	Sig es (a -> b) r -> Sig es' a r' ->
		Sig (es :+: es') b (ISig es (a -> b) r, ISig es' a r')
l <^> r = do
	(l', r') <- waitFor $ bothStart l r
	emitAll $ uncurry ($) `imap` pairs l' r'

bothStart :: (
	(es :+: es') ~ (es' :+: es),
	Collapse 'True (Occurred :$: (es :+: es')) (Occurred :$: es),
	Collapse 'True (Occurred :$: (es :+: es')) (Occurred :$: es'),
	Merge es es' (es :+: es'), Merge es' es (es :+: es')
	) => Sig es a r -> Sig es' b r' -> React (es :+: es') (ISig es a r, ISig es' b r')
l `bothStart` Sig r = do
	(Sig l', r') <- res $ l `until` r
	(Sig r'', l'') <- res $ Sig r' `until` l'
	pure (done' l'', done' r'')

done' :: React es a -> a
done' = fromJust . done

pairs :: Mergeable es es' => ISig es a r ->
	ISig es' b r' -> ISig (es :+: es') (a, b) (ISig es a r, ISig es' b r')
End a `pairs` b = pure (pure a, b)
a `pairs` End b = pure (a, pure b)
(hl :| Sig tl) `pairs` (hr :| Sig tr) = (hl, hr) :| tail
	where
	tail = Sig $ cont <$> tl `first` tr
	cont (tl', tr') = lup hl tl' `pairs` lup hr tr'
	lup _ (Done l) = l
	lup h t = h :| Sig t

indexBy ::
	Mergeable es es' => Sig es a r -> Sig es' b r' -> Sig (es :+: es') a ()
l `indexBy` Sig r = waitFor (res $ l `until` r) >>= \case
	(_, Done (End _)) -> pure ()
	(Sig (Done l'), r') -> l' `iindexBy` Sig r'
	(Sig l', Done (_ :| r')) -> Sig l' `indexBy` r'
	_ -> error "never occur"

iindexBy ::
	Mergeable es es' => ISig es a r -> Sig es' b r' -> Sig (es :+: es') a ()
l `iindexBy` Sig r = waitFor (ires $ l `iuntil` r) >>= \case
	(hl :| tl, Done (_ :| tr)) -> emit hl >> (hl :| tl) `iindexBy` tr
	_ -> pure ()

spawn :: Sig es a r -> Sig es (ISig es a r) ()
spawn (Sig l) = repeat l

parList :: (
	Nihil es', (es :+: es') ~ es', (es' :+: es') ~ es',
	Mergeable 'Nil es', Mergeable es' es, Mergeable es' es') =>
	Sig es (ISig es' a r) r' -> Sig es' [a] ()
parList x = emitAll $ iparList x

iparList :: (
	Nihil es', (es' :+: es') ~ es',
	(es :+: es') ~ es',
	Merge es' es' es', 
	Mergeable 'Nil es',
	Mergeable es' es
	) => Sig es (ISig es' a r) r' -> ISig es' [a] ()
iparList l = () <$ (rl ([] :| hold) l) where
	rl t (Sig es) = do
		(t', es') <- t `iuntil` es
		case es' of
			Done (e'' :| es'') -> rl (cons e'' t') es''
			_ -> t'

cons :: ((es :+: es) ~ es, Mergeable es es) =>
	ISig es a r -> ISig es [a] r' -> ISig es [a] ()
cons h t = () <$ do
	(h', t') <- uncurry (:) `imap` pairs h t
	void $ (: []) `imap` h'
	void t'
