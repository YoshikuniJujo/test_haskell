{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.ReactNew where

import Data.Kind
import Data.Type.Set
import Data.OneOrMore
import Data.Or
import Data.Bits
import Numeric.Natural

import qualified Control.Arrow as A

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

runReactSt :: Monad m => st -> ThreadId -> HandleSt st m es -> React s es a -> m ((a, ThreadId), st)
runReactSt st ti _ (Pure x) = pure ((x, ti), st)
runReactSt _ _ _ (Never :>>= _) = error "never end"
runReactSt st ti hdl (GetThreadId :>>= c) = runReactSt st ti hdl (c `qApp` ti)
runReactSt st _ hdl (PutThreadId ti' :>>= c) = runReactSt st ti' hdl (c `qApp` ())
runReactSt st ti hdl (Await rqs :>>= c) = do
	(x, st') <- hdl st rqs
	runReactSt st' ti hdl (c `qApp` x)

interpretReactSt :: Monad m => st -> HandleSt st m es -> React s es a -> m (a, st)
interpretReactSt st0 hdl r = (fst `A.first`) <$> runReactSt st0 rootThreadId hdl r

type Handle m es = EvReqs es -> m (EvOccs es)
type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))
type HandleSt st m es = st -> EvReqs es -> m (EvOccs es, st)
type HandleSt' st st' m es = st -> EvReqs es -> m (Maybe (EvOccs es), st')

await :: a -> (Occurred a -> b) -> React s (Singleton a) b
await r f = Await (singleton r) >>>= (pure . f . extract)

await' :: a -> (ThreadId -> Occurred a -> b) -> React s (Singleton a) b
await' r f = Await (singleton r) >>>= \o -> do
	ti <- getThreadId
	pure . f ti $ extract o

type ExpandableOccurred es es' = Expandable (Occurred :$: es) (Occurred :$: es')
type CollapsableOccurred es es' = Collapsable (Occurred :$: es) (Occurred :$: es')
type MergeableOccurred es es' eses' = Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: eses')

class Update a b where
	update ::
		React s es a -> ThreadId ->
		React s es b -> ThreadId ->
		EvOccs es -> (React s es a, React s es b)

instance {-# OVERLAPPABLE #-} Update a b where
	update (GetThreadId :>>= c) ti r' ti' b = update (c `qApp` ti) ti r' ti' b
	update r ti (GetThreadId :>>= c') ti' b = update r ti (c' `qApp` ti') ti' b
	update (PutThreadId ti :>>= c) _ r' ti' b = update (c `qApp` ()) ti r' ti' b
	update r ti (PutThreadId ti' :>>= c') _ b = update r ti (c' `qApp` ()) ti' b
	update (Await _ :>>= c) _ (Await _ :>>= c') _ b = (c `qApp` b, c' `qApp` b)
	update r@(Never :>>= _) _ (Await _ :>>= c') _ b = (r, c' `qApp` b)
	update (Await _ :>>= c) _ r'@(Never :>>= _) _ b = (c `qApp` b, r')
	update r _ r' _ _ = (r, r')

instance Update a a where
	update (GetThreadId :>>= c) ti r' ti' b = update (c `qApp` ti) ti r' ti' b
	update r ti (GetThreadId :>>= c') ti' b = update r ti (c' `qApp` ti') ti' b
	update (PutThreadId ti :>>= c) _ r' ti' b = update (c `qApp` ()) ti r' ti' b
	update r ti (PutThreadId ti' :>>= c') _ b = update r ti (c' `qApp` ()) ti' b
	update (Await _ :>>= c) _ (Await _ :>>= c') _ b = qAppPar c c' b
	update r _ r' _ _ = (r, r')

adjust :: (Expandable es es', CollapsableOccurred es' es) =>
	React s es a -> React s es' a
adjust = \case
	Pure x -> pure x
	Never :>>= _ -> Never >>>= pure
	l@(Await e :>>= c) -> Await (expand e) >>>= \occ -> case collapse occ of
		Just occ' -> adjust $ c `qApp` occ'
		Nothing -> adjust l
	GetThreadId :>>= c -> GetThreadId >>>= \ti -> adjust $ c `qApp` ti
	PutThreadId ti :>>= c -> PutThreadId ti >>>= \() -> adjust $ c `qApp` ()

never :: React s es a
never = Never >>>= pure

first :: (
	Update a b,
	Expandable es (es :+: es'), Expandable es' (es :+: es'), 
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	Mergeable (es :+: es') (es :+: es') (es :+: es')
	) => React s es a -> React s es' b -> React s (es :+: es') (Or a b)
l `first` r = (<$> adjust l `par` adjust r) \case
	(Pure x, Pure y) -> LR x y
	(Pure x, _) -> L x; (_, Pure y) -> R y
	(_ :>>= _, _:>>= _) -> error "never occur"

par :: (Update a b, Mergeable es es es) =>
	React s es a -> React s es b -> React s es (React s es a, React s es b)
l `par` r = case (l, r) of
	(Never :>>= _, Never :>>= _) -> never
	(Pure _, _) -> pure (l, r)
	(_, Pure _) -> pure (l, r)
	(Never :>>= _, _) -> (never ,) . Pure <$> r
	(_, Never :>>= _) -> (, never) . Pure <$> l
	(GetThreadId :>>= c, r') -> do
		ti <- getThreadId
		let	(ti1, _ti2) = forkThreadId ti
		(c `qApp` ti1) `par` r'
	(l', GetThreadId :>>= c') -> do
		ti <- getThreadId
		let	(_ti1, ti2) = forkThreadId ti
		l' `par` (c' `qApp` ti2)
	(PutThreadId ti :>>= c, r') ->
		(A.first (putThreadId ti >>)) <$> (c `qApp` ()) `par` r'
	(l', PutThreadId ti' :>>= c') ->
		(A.second (putThreadId ti' >>)) <$> l' `par` (c' `qApp` ())
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `merge` er
		c b ti = let
			(ti1, ti2) = forkThreadId ti
			(u, u') = update l ti1 r ti2 b in u `par` u' in do
			o <- Await e >>>= pure
			ti <- getThreadId
			c o ti
