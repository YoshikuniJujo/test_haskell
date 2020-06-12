{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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
	-- * Parallel Composition
	at, break, until, indexBy
	) where

import Prelude hiding (repeat, scanl, break, until)

import GHC.Stack (HasCallStack)
import Control.Arrow ((***))
import Control.Monad (void)
import Data.Type.Flip (Flip(..), (<$%>), (<*%>))
import Data.Type.Set ((:+:))
import Data.UnionSet (Mergeable)

import MonadicFrp.React.Internal (
	React(..), Adjustable, Firstable, Handle, HandleSt,
	adjust, first_, interpretReact, interpretReactSt )

---------------------------------------------------------------------------

-- * TYPE SIG AND ISIG
--	+ HOLD AND PAIRS
--	+ MONAD
--	+ FLIP APPLICATIVE
--		- Sig
--		- ISig
-- * INTERPRET
-- * COMBINATOR
--	+ CONVERSION
--	+ TRANSFORMATION
--	+ REPETITION
--	+ PARALLEL COMPOSITION

---------------------------------------------------------------------------
-- TYPE SIG AND ISIG
---------------------------------------------------------------------------

infixr 5 :|
newtype Sig es a r = Sig { unSig :: React es (ISig es a r) }
data ISig es a r = End r | a :| Sig es a r

isig :: (r -> b) -> (a -> Sig es a r -> b) -> ISig es a r -> b
isig e c = \case End x -> e x; h :| t -> c h t

-- HOLD AND PAIRS

hold :: Sig es a r
hold = waitFor Never

pairs :: Firstable es es' => ISig es a r -> ISig es' b r' ->
	ISig (es :+: es') (a, b) (ISig es a r, ISig es' b r')
End l `pairs` r = pure (pure l, r)
l `pairs` End r = pure (l, pure r)
(hl :| Sig tl) `pairs` (hr :| Sig tr) = (hl, hr) :| t'
	where
	t' = Sig $ uncurry pairs . ((hl ?:|) *** (hr ?:|)) <$> tl `first_` tr
	(?:|) h = \case Done t -> t; t -> h :| Sig t

-- MONAD

instance Functor (Sig es a) where f `fmap` Sig s = Sig $ (f <$>) <$> s

instance Applicative (Sig es a) where
	pure = emitAll . pure
	Sig rf <*> mx = Sig $ isig
		(unSig . (<$> mx)) (\h -> pure . (h :|) . ((<$> mx) =<<)) =<< rf

instance Monad (Sig es a) where
	Sig r >>= f =
		Sig $ isig (unSig . f) (\h -> pure . (h :|) . (f =<<)) =<< r

instance Functor (ISig es a) where
	fmap f = isig (End . f) (\h -> (h :|) . (f <$>))

instance Applicative (ISig es a) where
	pure = End
	mf <*> mx = isig (<$> mx) (\h -> (h :|) . (emitAll . (<$> mx) =<<)) mf

instance Monad (ISig es a) where
	m >>= f = isig f (\h -> (h :|) . (emitAll . f =<<)) m

-- FLIP APPLICATIVE

-- Sig

instance Functor (Flip (Sig es) r) where
	fmap f = Flip . Sig . ((f <$%>) <$>) . unSig . unflip

instance ((es :+: es) ~ es, Firstable es es, Semigroup r) =>
	Applicative (Flip (Sig es) r) where
	pure = Flip . (>> hold) . emit
	mf <*> mx = Flip $ unflip mf `app` unflip mx

app :: (HasCallStack, (es :+: es) ~ es, Firstable es es, Semigroup r) =>
	Sig es (a -> b) r -> Sig es a r -> Sig es b r
mf `app` mx = emitAll . uncurry (<*%>) =<< waitFor (mf `bothStart` mx)

bothStart :: (
	HasCallStack, Firstable es es',
	Mergeable es es' (es :+: es'), Mergeable es' es (es :+: es') ) =>
	Sig es a r -> Sig es' b r' ->
	React (es :+: es') (ISig es a r, ISig es' b r')
l `bothStart` Sig r = do
	(Sig l', r') <- res $ l `until_` r
	(Sig r'', l'') <- res $ Sig r' `until_` l'
	pure (ex l'', ex r'')
	where
	ex :: HasCallStack => React es a -> a
	ex = \case
		Done x -> x
		Await _ _ -> error "Await _ _"; Never -> error "Never"

-- ISig

instance Functor (Flip (ISig es) r) where
	fmap f = Flip . isig pure (\h -> (f h :|) . (f <$%>)) . unflip

instance ((es :+: es) ~ es, Firstable es es, Semigroup r) =>
	Applicative (Flip (ISig es) r) where
	pure = Flip . (:| hold)
	mf <*> mx = Flip $ unflip mf `iapp` unflip mx

iapp :: ((es :+: es) ~ es, Firstable es es, Semigroup r) =>
	ISig es (a -> b) r -> ISig es a r -> ISig es b r
mf `iapp` mx = (<$> (uncurry ($) <$%> mf `pairs` mx)) \case
	(End x, End y) -> x <> y; (End x, _ :| _) -> x; (_ :| _, End y) -> y
	(_ :| _, _ :| _) -> error "never occur"

---------------------------------------------------------------------------
-- INTERPRET
---------------------------------------------------------------------------

interpret :: Monad m => Handle m es -> (a -> m ()) -> Sig es a r -> m r
interpret p d = go where
	go (Sig s) = interpretReact p s >>= isig pure \h -> (d h >>) . go

interpretSt ::
	Monad m => st -> HandleSt st m es -> (a -> m ()) -> Sig es a r -> m r
interpretSt st0 p d = go st0 where
	go st (Sig s) = do
		(x, st') <- interpretReactSt st p s
		isig pure (\h t -> d h >> go st' t) x

---------------------------------------------------------------------------
-- COMBINATOR
---------------------------------------------------------------------------

-- CONVERSION

emit :: a -> Sig es a ()
emit a = emitAll $ a :| pure ()

emitAll :: ISig es a b -> Sig es a b
emitAll = Sig . pure

waitFor :: React es r -> Sig es a r
waitFor = Sig . (pure <$>)

icur :: ISig es a b -> Either a b
icur (h :| _) = Left h
icur (End r) = Right r

res :: Sig es a b -> React es b
res (Sig l) = ires =<< l

ires :: ISig es a b -> React es b
ires (_ :| t) = res t
ires (End a) = pure a

-- TRANSFORMATION

scanl :: (b -> a -> b) -> b -> Sig es a r -> Sig es b r
scanl f i = emitAll . iscanl f i

iscanl :: (b -> a -> b) -> b -> Sig es a r -> ISig es b r
iscanl f i (Sig l) = i :| (waitFor l >>= lsl) where
	lsl (h :| t) = scanl f (f i h) t
	lsl (End x) = pure x

find :: (a -> Bool) -> Sig es a r -> React es (Either a r)
find p l = icur <$> res (brk p l)

brk :: (a -> Bool) -> Sig es a r -> Sig es a (ISig es a r)
brk p (Sig l) = Sig $ ibrk p <$> l

ibrk :: (a -> Bool) -> ISig es a r -> ISig es a (ISig es a r)
ibrk p is@(h :| t)
	| p h = pure is
	| otherwise = h :| brk p t
ibrk _ is@(End _) = pure is

-- REPETITION

repeat :: React es a -> Sig es a ()
-- repeat_ x = xs where xs = Sig $ (:| xs) <$> x
repeat x = waitFor x >>= emit >> repeat x

spawn :: Sig es a r -> Sig es (ISig es a r) ()
spawn (Sig l) = repeat l

parList :: ((es :+: es) ~ es, Firstable es es) => Sig es (ISig es a r) r' -> Sig es [a] ()
parList x = emitAll $ iparList x

iparList :: (
	(es' :+: es') ~ es',
	(es :+: es') ~ es',
	Mergeable es' es' es',
	Firstable es' es
	) => Sig es (ISig es' a r) r' -> ISig es' [a] ()
iparList = rl ([] :| hold) where
	rl t (Sig es) = do
		(t', es') <- t `iuntil` es
		case es' of
			Done (e'' :| es'') -> rl (cons e'' t') es''
			Done (End _) -> pure ()
			_ -> error "never occur"

cons :: ((es :+: es) ~ es, Firstable es es) =>
	ISig es a r -> ISig es [a] r' -> ISig es [a] ()
cons h t = () <$ do
	(h', t') <- uncurry (:) <$%> h `pairs` t
	void $ (: []) <$%> h'
	void t'

-- PARALLEL COMPOSITION

infixr 7 `at`

at :: (Firstable es es', Adjustable es (es :+: es')) =>
	Sig es a r -> React es' r' -> React (es :+: es') (Either r (a, r'))
l `at` a = do
	(l', r') <- res $ l `until_` a
	case (l', r') of
		(Sig (Done (h :| _)), Done r'') -> pure $ Right (h, r'')
		(Sig (Done (End l'')), _) -> pure . Left $ l''
		(Sig c@(Await _ _), Done r'') -> adjust c >>= \case
			aa :| _ -> pure $ Right (aa, r'')
			End rr -> pure $ Left rr
		_ -> error "never occur"

infixl 7 `break`, `until`

break :: Firstable es es' =>
	Sig es a r -> React es' r' -> Sig (es :+: es') a (Either r (Maybe (Either a r), r'))
l `break` r = do
	(l', r') <- l `until_` r
	case (l', r') of
		(Sig (Done (l'' :| _)), Done r'') -> pure $ Right (Just $ Left l'', r'')
		(Sig (Done (End l'')), Done r'') -> pure $ Right (Just $ Right l'', r'')
		(Sig (Done (End l'')), _) -> pure $ Left l''
		(Sig (Await _ _), Done r'') -> pure $ Right (Nothing, r'')
		_ -> error "never occur"

until :: (Firstable es es', Adjustable es (es :+: es')) =>
	Sig es a r -> React es' r' -> Sig (es :+: es') a (Either r (Either a r, r'))
l `until` r = do
	(l', r') <- l `until_` r
	case (l', r') of
		(Sig (Done (l'' :| _)), Done r'') -> pure $ Right (Left l'', r'')
		(Sig (Done (End l'')), Done r'') -> pure $ Right (Right l'', r'')
		(Sig (Done (End l'')), _) -> pure $ Left l''
		(Sig c@(Await _ _), Done r'') -> waitFor (adjust c) >>= \case
			a :| _ -> pure $ Right (Left a, r'')
			End rr -> pure $ Right (Right rr, r'')
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

infixl 7 `indexBy`

indexBy :: (Firstable es es', Adjustable es (es :+: es')) =>
	Sig es a r -> Sig es' b r' -> Sig (es :+: es') a (Either r (Either a r, r'))
l `indexBy` Sig r = waitFor (res $ l `until_` r) >>= \case
	(Sig (Done l'), r') -> l' `iindexBy` Sig r'
	(Sig l', Done (_ :| r')) -> Sig l' `indexBy` r'
	(Sig c@(Await _ _), Done (End r'')) -> waitFor (adjust c) >>= \case
		a :| _ -> pure $ Right (Left a, r'')
		End rr -> pure $ Right (Right rr, r'')
	_ -> error "never occur"

iindexBy ::
	Firstable es es' => ISig es a r -> Sig es' b r' -> Sig (es :+: es') a (Either r (Either a r, r'))
l `iindexBy` Sig r = waitFor (ires $ l `iuntil` r) >>= \case
	(hl :| tl, Done (_ :| tr)) -> emit hl >> (hl :| tl) `iindexBy` tr
	(hl :| _, Done (End r')) -> pure $ Right (Left hl, r')
	(End l', _) -> pure $ Left l'
	_ -> error "never occur"
