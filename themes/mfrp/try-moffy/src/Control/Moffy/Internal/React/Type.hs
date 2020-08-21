{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.React.Type (
	ExpandableOccurred, CollapsableOccurred, MergeableOccurred,
	-- * React
	-- ** Type
	React, Rct(..), Request(..), EvReqs, EvOccs,
	-- ** Never and Await
	never, await, await',
	-- * Handle
	Handle, HandleSt, liftHandle,
	-- * St
	St, liftSt,
	-- * ThreadId
	ThreadId, rootThreadId, forkThreadId ) where

import Control.Monad.Freer.Par (Freer, (>>>=), (=<<<))
import Control.Monad.Freer.Par.FTCQueue (FTCQueue)
import Control.Monad.Freer.Par.TaggableFunction (TaggableFun)
import Data.Kind (Type)
import Data.Type.Set (Set, Numbered, Singleton, (:$:))
import Data.OneOrMore (
	OneOrMore, Selectable, pattern Singleton, unSingleton,
	Expandable, Collapsable, Mergeable )
import Data.Bits (setBit)
import Numeric.Natural (Natural)

---------------------------------------------------------------------------

-- * REACT
--	+ TYPE
--	+ NEVER AND AWAIT
-- * HANDLE
-- * ST
-- * THREAD ID

---------------------------------------------------------------------------
-- FOO
---------------------------------------------------------------------------

type ExpandableOccurred es es' = Expandable (Occurred :$: es) (Occurred :$: es')

type CollapsableOccurred es es' =
	Collapsable (Occurred :$: es) (Occurred :$: es')

type MergeableOccurred es es' mrg =
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: mrg)

---------------------------------------------------------------------------
-- REACT
---------------------------------------------------------------------------

-- TYPE

type React s es r = Freer s FTCQueue TaggableFun (Rct es) r

data Rct es r where
	Never :: Rct es r; GetThreadId :: Rct es ThreadId
	Await :: EvReqs es -> Rct es (EvOccs es)

type EvReqs (es :: Set Type) = OneOrMore es
type EvOccs (es :: Set Type) = OneOrMore (Occurred :$: es)

class (Numbered e, Selectable e) => Request e where data Occurred e

-- NEVER AND AWAIT

never :: React s es a
never = pure =<<< Never

await :: e -> (Occurred e -> r) -> React s (Singleton e) r
await rq f = pure . f . unSingleton =<<< Await (Singleton rq)

await' :: e -> (ThreadId -> Occurred e -> r) -> React s (Singleton e) r
await' rq f = await rq . f =<<< GetThreadId

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

type Handle m es = EvReqs es -> m (EvOccs es)
type HandleSt st m es = EvReqs es -> St st m (EvOccs es)

liftHandle :: Functor m => Handle m es -> HandleSt st m es
liftHandle = (liftSt .)

---------------------------------------------------------------------------
-- ST
---------------------------------------------------------------------------

type St st m a = st -> m (a, st)

liftSt :: Functor m => m r -> St st m r
liftSt m = (<$> m) . flip (,)

---------------------------------------------------------------------------
-- THREAD ID
---------------------------------------------------------------------------

data ThreadId = ThreadId Natural Int deriving (Show, Eq)

rootThreadId :: ThreadId
rootThreadId = ThreadId 0 0

forkThreadId :: React s es (ThreadId, ThreadId)
forkThreadId = GetThreadId >>>= \(ThreadId n i) ->
	pure (ThreadId n $ i + 1, ThreadId (n `setBit` i) $ i + 1)
