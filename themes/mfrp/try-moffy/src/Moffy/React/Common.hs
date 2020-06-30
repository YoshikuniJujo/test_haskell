{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.React.Common where

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

never :: React s es a
never = Never >>>= pure
