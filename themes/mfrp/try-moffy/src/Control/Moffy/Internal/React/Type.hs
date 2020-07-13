{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.React.Type (
	-- * Type
	-- ** Basic
	React, Rct(..), Request(..), EvReqs, EvOccs,
	-- ** Handle
	Handle, Handle', HandleSt, HandleSt',
	-- ** Occurred Context
	ExpandableOccurred, CollapsableOccurred, MergeableOccurred,
	-- * ThreadId
	ThreadId, rootThreadId, forkThreadId,
	-- * Basic Function
	await, await', never ) where

import Data.Kind
import Data.Type.Set
import Data.OneOrMore
import Data.Bits
import Numeric.Natural

import Control.Monad.Freer.Par
import Control.Monad.Freer.Par.FTCQueue
import Control.Monad.Freer.Par.TaggableFunction

type EvReqs (es :: Set Type) = OneOrMore es
type EvOccs (es :: Set Type) = OneOrMore (Occurred :$: es)

class (Numbered e, Selectable e) => Request e where
	data Occurred (e :: Type) :: Type

data Rct es a where
	Never :: Rct es a
	GetThreadId :: Rct es ThreadId
	Await :: EvReqs es -> Rct es (EvOccs es)
type React s es a = Freer s FTCQueue TaggableFun (Rct es) a

getThreadId :: React s es ThreadId
getThreadId = GetThreadId >>>= pure

data ThreadId = ThreadId Natural Int deriving (Show, Eq)

rootThreadId :: ThreadId
rootThreadId = ThreadId 0 0

forkThreadId :: React s es (ThreadId, ThreadId)
forkThreadId = forkThreadId_ <$> getThreadId

forkThreadId_ :: ThreadId -> (ThreadId, ThreadId)
forkThreadId_ (ThreadId n i) =
	(ThreadId n $ i + 1, ThreadId (n `setBit` i) $ i + 1)

step :: React s es a -> EvOccs es -> React s es a
step (Await _ :>>= c) = (c `qApp`)
step _ = error "not await"

await :: a -> (Occurred a -> b) -> React s (Singleton a) b
await r f = Await (singleton r) >>>= (pure . f . extract)

await' :: a -> (ThreadId -> Occurred a -> b) -> React s (Singleton a) b
await' r f = Await (singleton r) >>>= \o -> do
	ti <- getThreadId
	pure . f ti $ extract o

type ExpandableOccurred es es' = Expandable (Occurred :$: es) (Occurred :$: es')
type CollapsableOccurred es es' = Collapsable (Occurred :$: es) (Occurred :$: es')
type MergeableOccurred es es' mrg = Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: mrg)

never :: React s es a
never = Never >>>= pure

type Handle m es = EvReqs es -> m (EvOccs es)
type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))
type HandleSt st m es = st -> EvReqs es -> m (EvOccs es, st)
type HandleSt' st st' m es = st -> EvReqs es -> m (Maybe (EvOccs es), st')
