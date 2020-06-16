{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
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
import Control.Arrow ((***), first)
import Control.Monad (forever)
import Data.Type.Flip (Flip(..), (<$%>), (<*%>))
import Data.Type.Set ((:+:))
import Data.UnionSet (Mergeable)

import MonadicFrp.React.Internal (
	React(..), Adjustable, Firstable, Handle, HandleSt,
	adjust, first_, interpretReact, interpretReactSt )

---------------------------------------------------------------------------

-- * TYPE SIG AND ISIG
--	+ HOLD, PAIRS AND PAUSE
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

-- HOLD, PAIRS AND PAUSE

hold :: Sig es a r
hold = waitFor Never

ipairs :: Firstable es es' => ISig es a r -> ISig es' b r' ->
	ISig (es :+: es') (a, b) (ISig es a r, ISig es' b r')
l@(End _) `ipairs` r = pure (l, r)
l `ipairs` r@(End _) = pure (l, r)
(hl :| Sig tl) `ipairs` (hr :| Sig tr) = ((hl, hr) :|) . Sig
	$ uncurry ipairs . ((hl ?:|) *** (hr ?:|)) <$> tl `first_` tr
	where (?:|) h = \case Done t -> t; t -> h :| Sig t

pause :: Firstable es es' => Sig es a r -> React es' r' ->
	Sig (es :+: es') a (Sig es a r, React es' r')
Sig l `pause` r = waitFor (l `first_` r) >>= \case
	(Done l', r') -> (emitAll `first`) <$> emitAll (l' `ipause` r')
	(l', r'@(Done _)) -> pure (Sig l', r')
	(Await _ _, Await _ _) -> error "never occur"
	(Never, Await _ _) -> error "never occur"
	(_, Never) -> error "never occur"

ipause :: (HasCallStack, Firstable es es') => ISig es a r -> React es' r' ->
	ISig (es :+: es') a (ISig es a r, React es' r')
l@(End _) `ipause` r = pure (l, r)
(h :| t) `ipause` r = (h :|) $ (<$> (t `pause` r)) \case
	(Sig (Done t'), r') -> (t', r')
	(t', r'@(Done _)) -> (h :| t', r')
	(Sig (Await _ _), Await _ _) -> error "never occur"
	(Sig Never, Await _ _) -> error "never occur"
	(_, Never) -> error "never occur"

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
	(Sig l', r') <- res $ l `pause` r
	(Sig r'', l'') <- res $ Sig r' `pause` l'
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
mf `iapp` mx = (<$> (uncurry ($) <$%> mf `ipairs` mx)) \case
	(End x, End y) -> x <> y; (End x, _ :| _) -> x; (_ :| _, End y) -> y
	(_ :| _, _ :| _) -> error "never occur"

---------------------------------------------------------------------------
-- INTERPRET
---------------------------------------------------------------------------

interpret :: Monad m => Handle m es -> (a -> m ()) -> Sig es a r -> m r
interpret hdl vw = go where
	go (Sig s) = interpretReact hdl s >>= isig pure \h -> (vw h >>) . go

interpretSt ::
	Monad m => st -> HandleSt st m es -> (a -> m ()) -> Sig es a r -> m r
interpretSt st0 hdl vw = go st0 where
	go st (Sig s) = interpretReactSt st hdl s >>= \(is, st') ->
		isig pure (\h -> (vw h >>) . go st') is

---------------------------------------------------------------------------
-- COMBINATOR
---------------------------------------------------------------------------

-- CONVERSION

emit :: a -> Sig es a ()
emit = emitAll . (:| pure ())

emitAll :: ISig es a b -> Sig es a b
emitAll = Sig . pure

waitFor :: React es r -> Sig es a r
waitFor = Sig . (pure <$>)

icur :: ISig es a b -> Either a b
icur = isig Right (const . Left)

res :: Sig es a b -> React es b
res = (ires =<<) . unSig

ires :: ISig es a b -> React es b
ires = isig pure (const res)

-- TRANSFORMATION

scanl :: (b -> a -> b) -> b -> Sig es a r -> Sig es b r
scanl op v = emitAll . iscanl op v

iscanl :: (b -> a -> b) -> b -> Sig es a r -> ISig es b r
iscanl op v (Sig r) = v :| (isig pure (scanl op . (v `op`)) =<< waitFor r)

find :: (a -> Bool) -> Sig es a r -> React es (Either a r)
find p = (icur <$>) . res . brk
	where
	brk = Sig . (ibrk <$>) . unSig
	ibrk = \case
		is@(End _) -> pure is
		is@(h :| t) | p h -> pure is | otherwise -> h :| brk t

-- REPETITION

repeat :: React es a -> Sig es a ()
repeat = forever . (emit =<<) . waitFor

spawn :: Sig es a r -> Sig es (ISig es a r) ()
spawn  = repeat . unSig

parList :: ((es :+: es) ~ es, Firstable es es) =>
	Sig es (ISig es a r) r' -> Sig es [a] ([r], r')
parList (Sig r) = iparList =<< waitFor r

iparList :: ((es :+: es) ~ es, Firstable es es) =>
	ISig es (ISig es a r) r' -> Sig es [a] ([r], r')
iparList = isig (pure . ([] ,)) $ go . ((: []) <$>) . ((: []) <$%>) where
	go s (Sig r) = emitAll (s `ipause` r) >>= \case
		(s', Done (h :| t)) -> go (uncurry (:) <$> h `cons` s') t
		(s', Done (End y)) -> (, y) <$> emitAll s'
		(End x, r') -> emit [] >> ((x ++) `first`) <$> parList (Sig r')
		_ -> error "never occur"

cons :: ((es :+: es) ~ es, Firstable es es) =>
	ISig es a r -> ISig es [a] r' -> ISig es [a] (r, r')
h `cons` t = uncurry (:) <$%> h `ipairs` t >>=
	\(h', t') -> (,) <$> ((: []) <$%> h') <*> t'

-- PARALLEL COMPOSITION

infixr 7 `at`

at :: (Firstable es es', Adjustable es (es :+: es')) =>
	Sig es a r -> React es' r' -> React (es :+: es') (Either r (a, r'))
l `at` r = res (l `pause` r) >>= \(Sig l', r') -> (<$> adjust l') \case
	End x -> Left x
	h :| _ -> case r' of Done y -> Right (h, y); _ -> error "never occur"

infixl 7 `break`, `until`

break :: Firstable es es' =>
	Sig es a r -> React es' r' -> Sig (es :+: es') a (Either r (Maybe (Either a r), r'))
l `break` r = do
	(l', r') <- l `pause` r
	case (l', r') of
		(Sig (Done (l'' :| _)), Done r'') -> pure $ Right (Just $ Left l'', r'')
		(Sig (Done (End l'')), Done r'') -> pure $ Right (Just $ Right l'', r'')
		(Sig (Done (End l'')), _) -> pure $ Left l''
		(Sig (Await _ _), Done r'') -> pure $ Right (Nothing, r'')
		_ -> error "never occur"

until :: (Firstable es es', Adjustable es (es :+: es')) =>
	Sig es a r -> React es' r' -> Sig (es :+: es') a (Either r (Either a r, r'))
l `until` r = do
	(l', r') <- l `pause` r
	case (l', r') of
		(Sig (Done (l'' :| _)), Done r'') -> pure $ Right (Left l'', r'')
		(Sig (Done (End l'')), Done r'') -> pure $ Right (Right l'', r'')
		(Sig (Done (End l'')), _) -> pure $ Left l''
		(Sig c@(Await _ _), Done r'') -> waitFor (adjust c) >>= \case
			a :| _ -> pure $ Right (Left a, r'')
			End rr -> pure $ Right (Right rr, r'')
		_ -> error "never occur"

infixl 7 `indexBy`

indexBy :: (Firstable es es', Adjustable es (es :+: es')) =>
	Sig es a r -> Sig es' b r' -> Sig (es :+: es') a (Either r (Either a r, r'))
l `indexBy` Sig r = waitFor (res $ l `pause` r) >>= \case
	(Sig (Done l'), r') -> l' `iindexBy` Sig r'
	(Sig l', Done (_ :| r')) -> Sig l' `indexBy` r'
	(Sig c@(Await _ _), Done (End r'')) -> waitFor (adjust c) >>= \case
		a :| _ -> pure $ Right (Left a, r'')
		End rr -> pure $ Right (Right rr, r'')
	_ -> error "never occur"

iindexBy ::
	Firstable es es' => ISig es a r -> Sig es' b r' -> Sig (es :+: es') a (Either r (Either a r, r'))
l `iindexBy` Sig r = waitFor (ires $ l `ipause` r) >>= \case
	(hl :| tl, Done (_ :| tr)) -> emit hl >> (hl :| tl) `iindexBy` tr
	(hl :| _, Done (End r')) -> pure $ Right (Left hl, r')
	(End l', _) -> pure $ Left l'
	_ -> error "never occur"
