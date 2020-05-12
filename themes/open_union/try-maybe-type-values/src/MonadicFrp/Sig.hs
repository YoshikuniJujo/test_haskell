{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGe FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.Sig (
	-- * Types
	Sig, ISig,
	-- * Run Sig
	interpret, interpretSt,
	-- * Conversion
	emit, waitFor,
	-- * Transformation
	scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel composition
	at, until, indexBy
	) where

import Prelude hiding (map, repeat, scanl, break, until, tail)

import GHC.Stack
import Control.Monad
import Data.Type.Flip
import Data.Maybe

import MonadicFrp.React.Internal
import Data.UnionSet
import Data.Type.Set hiding (Merge)

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

interpret :: Monad m => Handle m es -> (a -> m ()) -> Sig es a r -> m r
interpret p d = interpretSig' where
	interpretSig' (Sig s) = interpretReact p s >>= interpretISig
	interpretISig (End x) = pure x
--	interpretISig (h :| Never) = d h >> interpretSig' 
	interpretISig (h :| t) = d h >> interpretSig' t

interpretSt :: Monad m => st -> HandleSt m st es -> (a -> m ()) -> Sig es a r -> m r
interpretSt st0 p d = interpretSig st0 where
	interpretSig st (Sig s) = do
		(x, st') <- interpretReactSt st p s
		interpretISig st' x
	interpretISig _st (End x) = pure x
	interpretISig st (h :| t) = d h >> interpretSig st t

emitAll :: ISig es a b -> Sig es a b
emitAll = Sig . pure

emit :: a -> Sig es a ()
emit a = emitAll $ a :| pure ()

waitFor :: React es r -> Sig es a r
waitFor = Sig . (pure <$>)

repeat_, repeat :: React es a -> Sig es a ()
repeat_ x = xs where xs = Sig $ (:| xs) <$> x
repeat x = waitFor x >>= emit >> repeat x

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

infixr 7 `at`

at :: Firstable es es' =>
	Sig es a r -> React es' r' -> React (es :+: es') (Either (a, r') (Maybe r))
l `at` a = do
	(l', r') <- res $ l `until_` a
	pure $ case (l', r') of
		(Sig (Done (h :| _)), Done r'') -> Left (h, r'')
		(Sig (Done (End l'')), _) -> Right $ Just l''
		(Sig (Await _ _), Done _) -> Right Nothing
		_ -> error "never occur"

infixl 7 `until`

until :: Firstable es es' => Sig es a r -> React es' r' -> Sig (es :+: es') a (Either (a, r') (Maybe r))
l `until` r = do
	(l', r') <- l `until_` r
	pure case (l', r') of
		(Sig (Done (l'' :| _)), Done r'') -> Left (l'', r'')
		(Sig (Done (End l'')), _) -> Right $ Just l''
		(Sig (Await _ _), Done _) -> Right Nothing
		_ -> error "never occur"

until_ :: Firstable es es' => Sig es a r ->
	React es' r' -> Sig (es :+: es') a (Sig es a r, React es' r')
Sig l `until_` a = waitFor (l `first_` a) >>= un where
	un (Done l', a') = do
		(l'', a'') <- emitAll $ l' `iuntil` a'
		pure (emitAll l'', a'')
	un (l', a') = pure (Sig l', a')

iuntil :: (HasCallStack, Firstable es es') => ISig es a r ->
	React es' r' -> ISig (es :+: es') a (ISig es a r, React es' r')
End l `iuntil` a = pure (pure l, a)
(h :| Sig t) `iuntil` a = h :| Sig (cont <$> t `first_` a) where
	cont (Done l', a') = l' `iuntil` a'
	cont (t', Done a') = pure (h :| Sig t', pure a')
	cont _ = error "never occur"

hold :: Sig es a r
hold = waitFor Never

always :: a -> Sig es a r
always a = emit a >> hold

(<^>) :: (Firstable es es', Firstable es' es) =>
	Sig es (a -> b) r -> Sig es' a r' ->
		Sig (es :+: es') b (ISig es (a -> b) r, ISig es' a r')
l <^> r = do
	(l', r') <- waitFor $ bothStart l r
	emitAll $ uncurry ($) `imap` pairs l' r'

bothStart :: (
	(es :+: es') ~ (es' :+: es),
	Expandable es (es :+: es'),
	Expandable es' (es :+: es'),
	Collapsable (Occurred :$: (es :+: es')) (Occurred :$: es),
	Collapsable (Occurred :$: (es :+: es')) (Occurred :$: es'),
	Mergeable es es' (es :+: es'), Mergeable es' es (es :+: es')
	) => Sig es a r -> Sig es' b r' -> React (es :+: es') (ISig es a r, ISig es' b r')
l `bothStart` Sig r = do
	(Sig l', r') <- res $ l `until_` r
	(Sig r'', l'') <- res $ Sig r' `until_` l'
	pure (done' l'', done' r'')

done' :: React es a -> a
done' = fromJust . done

pairs :: Firstable es es' => ISig es a r ->
	ISig es' b r' -> ISig (es :+: es') (a, b) (ISig es a r, ISig es' b r')
End a `pairs` b = pure (pure a, b)
a `pairs` End b = pure (a, pure b)
(hl :| Sig tl) `pairs` (hr :| Sig tr) = (hl, hr) :| tail
	where
	tail = Sig $ cont <$> tl `first_` tr
	cont (tl', tr') = lup hl tl' `pairs` lup hr tr'
	lup _ (Done l) = l
	lup h t = h :| Sig t

infixl 7 `indexBy`

indexBy ::
	Firstable es es' => Sig es a r -> Sig es' b r' -> Sig (es :+: es') a (Either (a, r') (Maybe r))
l `indexBy` Sig r = waitFor (res $ l `until_` r) >>= \case
	(Sig (Done l'), r') -> (Just <$>) <$> l' `iindexBy'` Sig r'
	(Sig l', Done (_ :| r')) -> Sig l' `indexBy` r'
	(Sig _, Done (End _)) -> pure $ Right Nothing
	_ -> error "never occur"

_iuntil' :: Firstable es es' => ISig es a r ->
	React es' r' -> ISig (es :+: es') a (Either a r)
l `_iuntil'` r = (<$> l `iuntil` r) \case
	(h :| _, _) -> Left h
	(End l', _) -> Right l'

iindexBy' ::
	Firstable es es' => ISig es a r -> Sig es' b r' -> Sig (es :+: es') a (Either (a, r') r)
l `iindexBy'` Sig r = waitFor (ires $ l `iuntil` r) >>= \case
	(hl :| tl, Done (_ :| tr)) -> emit hl >> (hl :| tl) `iindexBy'` tr
	(hl :| _, Done (End r')) -> pure $ Left (hl, r')
	(End l', _) -> pure $ Right l'
	_ -> error "never occur"

spawn :: Sig es a r -> Sig es (ISig es a r) ()
spawn (Sig l) = repeat_ l

parList :: ((es :+: es) ~ es, Firstable es es) => Sig es (ISig es a r) r' -> Sig es [a] ()
parList = parList_

parList_ :: (
	(es :+: es') ~ es', (es' :+: es') ~ es',
	Firstable es' es, Firstable es' es') =>
	Sig es (ISig es' a r) r' -> Sig es' [a] ()
parList_ x = emitAll $ iparList x

iparList :: (
	(es' :+: es') ~ es',
	(es :+: es') ~ es',
	Mergeable es' es' es',
	Firstable es' es
	) => Sig es (ISig es' a r) r' -> ISig es' [a] ()
iparList l = rl ([] :| hold) l where
	rl t (Sig es) = do
		(t', es') <- t `iuntil` es
		case es' of
			Done (e'' :| es'') -> rl (cons e'' t') es''
			Done (End _) -> pure ()
			_ -> error "never occur"

cons :: ((es :+: es) ~ es, Firstable es es) =>
	ISig es a r -> ISig es [a] r' -> ISig es [a] ()
cons h t = () <$ do
	(h', t') <- uncurry (:) `imap` pairs h t
	void $ (: []) `imap` h'
	void t'

instance Functor (Flip (Sig es) r) where
	fmap f = Flip . map f . unflip

instance (
	(es :+: es) ~ es, Firstable es es,
	Semigroup r ) => Applicative (Flip (Sig es) r) where
	pure = Flip . always
	mf <*> mx = Flip $ unflip mf `app` unflip mx

app :: (
	(es :+: es) ~ es,
	Mergeable es es es,
	Expandable es es,
	Collapsable (Occurred :$: es) (Occurred :$: es),
	Semigroup r ) => Sig es (a -> b) r -> Sig es a r -> Sig es b r
mf `app` mx = do
	(l, r) <- mf <^> mx
	case (l, r) of
		(End x, End y) -> pure $ x <> y
		(End x, _ :| _) -> pure x
		(_ :| _, End y) -> pure y
		(_ :| _, _ :| _) -> error "never occur"
