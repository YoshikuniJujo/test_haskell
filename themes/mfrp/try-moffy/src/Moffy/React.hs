{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.React where

import Data.Kind
import Data.Type.Set
import Data.OneOrMore
import Data.Or
import Data.Bits
import Numeric.Natural

import Freer
import FTCQueue
import TaggableFunction

type EvReqs (es :: Set Type) = OneOrMore es
type EvOccs (es :: Set Type) = OneOrMore (Occurred :$: es)

class (Numbered e, Selectable e) => Request e where
	data Occurred (e :: Type) :: Type

data Rct es a where
	Never :: Rct es a
	GetThreadId :: Rct es ThreadId
	PutThreadId :: ThreadId -> Rct es ()
	Await :: EvReqs es -> Rct es (EvOccs es)
type React s es a = Freer s FTCQueue (Taggable s) (Rct es) a

getThreadId :: React s es ThreadId
getThreadId = GetThreadId >>>= pure

putThreadId :: ThreadId -> React s es ()
putThreadId ti = PutThreadId ti >>>= pure

data ThreadId = ThreadId Natural Int deriving (Show, Eq)

rootThreadId :: ThreadId
rootThreadId = ThreadId 0 0

forkThreadId :: ThreadId -> (ThreadId, ThreadId)
forkThreadId (ThreadId n i) =
	(ThreadId n $ i + 1, ThreadId (n `setBit` i) $ i + 1)

runReact :: Monad m => ThreadId -> Handle m es -> React s es a -> m (a, ThreadId)
runReact ti _ (Pure x) = pure (x, ti)
runReact _ _ (Never :>>= _) = error "never end"
runReact ti hdl (GetThreadId :>>= c) = runReact ti hdl (c `qApp` ti)
runReact _ hdl (PutThreadId ti' :>>= c) = runReact ti' hdl (c `qApp` ())
runReact ti hdl (Await rqs :>>= c) = runReact ti hdl . (c `qApp`) =<< hdl rqs

interpretReact :: Monad m => Handle m es -> React s es a -> m a
interpretReact hdl r = fst <$> runReact rootThreadId hdl r

type Handle m es = EvReqs es -> m (EvOccs es)
type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))

retry :: Monad m => Handle' m es -> Handle m es
retry hdl rqs = maybe (retry hdl rqs) pure =<< hdl rqs

await :: a -> (Occurred a -> b) -> React s (Singleton a) b
await r f = Await (singleton r) >>>= (pure . f . extract)

first :: (
	Update es a es' b,
	Mergeable es es' (es :+: es')
	) => React s es a -> React s es' b -> React s (es :+: es') (Or a b)
l `first` r = (<$> l `par` r) \case
	(Pure l', Pure r') -> LR l' r'
	(Pure l', _) -> L l'; (_, Pure r') -> R r'
	_ -> error "never occur"

par ::	(
	Update es a es' b,
	Mergeable es es' (es :+: es')
	) => React s es a -> React s es' b -> React s (es :+: es') (React s es a, React s es' b)
l `par` r = case (l, r) of
	(Never :>>= _, Never :>>= _) -> error "never end"
--	(Done _, _) -> pure (l, r)
--	(_, Done _) -> pure (l, r)
--	(Never, Await er :>>= c) 
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `merge` er
		c b = let (u, u') = update l r b in u `par` u' in
		Await e >>>= c
	_ -> Pure (l, r)

type CollapsableOccurred es es' = Collapsable (Occurred :$: es) (Occurred :$: es')

type Updatable es a es' b = (
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	(Occurred :$: es) ~ (Occurred :$: es :+: es) )

class (	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	(Occurred :$: es) ~ (Occurred :$: es :+: es) ) =>
	Update es a es' b where
	update :: React s es a -> React s es' b -> EvOccs (es :+: es') -> (React s es a, React s es' b)

instance {-# OVERLAPPABLE #-} Updatable es a es' b => Update es a es' b where
	update r@(Await _ :>>= c) r'@(Await _ :>>= c') b = case (collapse b, collapse b) of
		(Just b', Just b'') -> (c `qApp` b', c' `qApp` b'')
		(Just b', Nothing) -> (c `qApp` b', r')
		(Nothing, Just b'') -> (r, c' `qApp` b'')
		(Nothing, Nothing) -> (r, r')
	update r@(Never :>>= _) r'@(Await _ :>>= c') b = case collapse b of
		Just b'' -> (r, c' `qApp` b'')
		Nothing -> (r, r')
	update r@(Await _ :>>= c) r'@(Never :>>= _) b = case collapse b of
		Just b' -> (c `qApp` b', r')
		Nothing -> (r, r')
	update r r' _ = (r, r')

instance Updatable es a es a => Update es a es a where
	update (Await _ :>>= c) (Await _ :>>= c') b = qAppPar c c' b
	update r r' _ = (r, r')

update' :: (
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es'
	) => React s es a -> React s es' b -> EvOccs (es :+: es') -> (React s es a, React s es' b)
update' r@(Await _ :>>= c) r'@(Await _ :>>= c') b = case (collapse b, collapse b) of
		(Just b', Just b'') -> (c `qApp` b', c' `qApp` b'')
		(Just b', Nothing) -> (c `qApp` b', r')
		(Nothing, Just b'') -> (r, c' `qApp` b'')
		(Nothing, Nothing) -> (r, r')
update' r@(Never :>>= _) r'@(Await _ :>>= c') b = case collapse b of
	Just b'' -> (r, c' `qApp` b'')
	Nothing -> (r, r')
update' r@(Await _ :>>= c) r'@(Never :>>= _) b = case collapse b of
	Just b' -> (c `qApp` b', r')
	Nothing -> (r, r')
update' r r' _ = (r, r')

par' ::	(
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	Mergeable es es' (es :+: es'),
	Expandable es (es :+: es'), Expandable es' (es :+: es')
	) => React s es a -> React s es' b -> React s (es :+: es') (React s es a, React s es' b)
l `par'` r = case (l, r) of
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `merge` er
		c b = let (u, u') = update' l r b in u `par'` u' in
		Await e >>>= c
	(Never :>>= _, Await er :>>= _) -> let
		e = expand er
		c b = let (u, u') = update' l r b in u `par'` u' in
		Await e >>>= c
	(Await el :>>= _, Never :>>= _) -> let
		e = expand el
		c b = let (u, u') = update' l r b in u `par'` u' in
		Await e >>>= c
	_ -> Pure (l, r)

adjust :: forall s es es' a . (
--	Update es a es' a,
	Mergeable es es' es',
	(es :+: es') ~ es',
	CollapsableOccurred es' es,
	CollapsableOccurred es' es',
	Expandable es es',
	Expandable es' es'
	) => React s es a -> React s es' a
adjust = \case
	Pure x -> pure x
	(Never :>>= _) -> Never >>>= pure
--	GetThreadId :>>= c -> GetThreadId :>>= adjust c
--	r@(Await _ :>>= _) -> (r `par'` (Never >>>= pure :: React s es' b)) >>= \case
	r -> (r `par'` (Never >>>= pure :: React s es' b)) >>= \case
		(Pure x, _) -> pure x
		(Await _ :>>= _, _) -> error "Await _ _"
		_ -> error "never occur"
