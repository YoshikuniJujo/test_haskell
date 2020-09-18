{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.React.Type (
	-- * React
	-- ** Type React and Data Rct
	React, Rct(..), EvReqs, EvOccs,
	-- ** Class Request
	Request(..),
	-- ** Constraint Synonym for Data Occurred
	ExpandableOccurred, CollapsableOccurred, MergeableOccurred,
	-- * Never and Await
	never, await, await',
	-- * Handle
	Handle, HandleSt, St, liftHandle, liftSt,
	-- * ThreadId
	ThreadId, rootThreadId, noThreadId, forkThreadId ) where

import Control.Monad.Freer.Par (Freer, (=<<<), (>>>=))
import Control.Monad.Freer.Par.FTCQueue (FTCQueue)
import Control.Monad.Freer.Par.TaggableFunction (TaggableFun)
import Data.Kind (Type)
import Data.Type.Set (Set, Numbered, Singleton)
import Data.OneOrMore (OneOrMore, Selectable, pattern Singleton)
import Data.Bits (setBit)
import Numeric.Natural (Natural)

import Data.Type.SetApp
import Data.OneOrMoreApp (
	OneOrMoreApp, Expandable, Collapsable, Mergeable, unSingleton )

---------------------------------------------------------------------------

-- * REACT
--	+ TYPE
--	+ NEVER AND AWAIT
-- * CONSTRAINT SYNONYM
-- * HANDLE
-- * THREAD ID

---------------------------------------------------------------------------
-- REACT
---------------------------------------------------------------------------

-- TYPE

type React s es = Freer s FTCQueue TaggableFun (Rct es)

data Rct es r where
	Never :: Rct es r; GetThreadId :: Rct es ThreadId
	Await :: EvReqs es -> Rct es (EvOccs es)

class (Numbered e, Selectable e) => Request e where data Occurred e

type EvReqs (es :: Set Type) = OneOrMore es
type EvOccs (es :: Set Type) = OneOrMoreApp (Occurred :$: es)

-- NEVER AND AWAIT

never :: React s es a
never = pure =<<< Never

await :: e -> (Occurred e -> r) -> React s (Singleton e) r
await rq f = pure . f . unSingleton =<<< Await (Singleton rq)

await' :: e -> (ThreadId -> Occurred e -> r) -> React s (Singleton e) r
await' rq f = await rq . f =<<< GetThreadId

---------------------------------------------------------------------------
-- CONSTRAINT SYNONYM
---------------------------------------------------------------------------

type ExpandableOccurred es es' = Expandable Occurred es es'

type CollapsableOccurred es es' = Collapsable Occurred es es'

type MergeableOccurred es es' mrg = Mergeable Occurred es es' mrg

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

type Handle m es = EvReqs es -> m (EvOccs es)
type HandleSt st m es = EvReqs es -> St st m (EvOccs es)
type St st m a = st -> m (a, st)

liftHandle :: Functor m => Handle m es -> HandleSt st m es
liftHandle = (liftSt .)

liftSt :: Functor m => m r -> St st m r
liftSt m = (<$> m) . flip (,)

---------------------------------------------------------------------------
-- THREAD ID
---------------------------------------------------------------------------

data ThreadId = NoThreadId | ThreadId Natural Int deriving (Show, Eq)

rootThreadId :: ThreadId
rootThreadId = ThreadId 0 0

noThreadId :: React s es (ThreadId, ThreadId)
noThreadId = pure (NoThreadId, NoThreadId)

forkThreadId :: React s es (ThreadId, ThreadId)
forkThreadId = GetThreadId >>>= \(ThreadId n i) ->
	pure (ThreadId n $ i + 1, ThreadId (n `setBit` i) $ i + 1)
