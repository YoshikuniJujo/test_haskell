{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.React.Type (
	-- * React
	React, Rct(..), Request(..), EvReqs, EvOccs,
	-- * Handle
	Handle, HandleSt,
	-- * ThreadId
	ThreadId, rootThreadId, forkThreadId,
	-- * Await and Never
	await, await', never ) where

import Control.Monad.Freer.Par (Freer, (>>>=), (=<<<))
import Control.Monad.Freer.Par.FTCQueue (FTCQueue)
import Control.Monad.Freer.Par.TaggableFunction (TaggableFun)
import Data.Kind (Type)
import Data.Type.Set (Set, Numbered, Singleton, (:$:))
import Data.OneOrMore (OneOrMore, Selectable, extract, singleton)
import Data.Bits (setBit)
import Numeric.Natural (Natural)

---------------------------------------------------------------------------

-- * REACT AND HANDLE
-- * THREAD ID
-- * AWAIT AND NEVER

---------------------------------------------------------------------------
-- REACT AND HANDLE
---------------------------------------------------------------------------

type React s es a = Freer s FTCQueue TaggableFun (Rct es) a

type Handle m es = EvReqs es -> m (EvOccs es)
type HandleSt st m es = st -> EvReqs es -> m (EvOccs es, st)

data Rct es a where
	Never :: Rct es a; GetThreadId :: Rct es ThreadId
	Await :: EvReqs es -> Rct es (EvOccs es)

class (Numbered e, Selectable e) => Request e where data Occurred e

type EvReqs (es :: Set Type) = OneOrMore es
type EvOccs (es :: Set Type) = OneOrMore (Occurred :$: es)

---------------------------------------------------------------------------
-- THREAD ID
---------------------------------------------------------------------------

data ThreadId = ThreadId Natural Int deriving (Show, Eq)

rootThreadId :: ThreadId
rootThreadId = ThreadId 0 0

forkThreadId :: React s es (ThreadId, ThreadId)
forkThreadId = forkThreadId_ <$> pure =<<< GetThreadId

forkThreadId_ :: ThreadId -> (ThreadId, ThreadId)
forkThreadId_ (ThreadId n i) =
	(ThreadId n $ i + 1, ThreadId (n `setBit` i) $ i + 1)

---------------------------------------------------------------------------
-- AWAIT AND NEVER
---------------------------------------------------------------------------

await :: a -> (Occurred a -> b) -> React s (Singleton a) b
await r f = Await (singleton r) >>>= (pure . f . extract)

await' :: a -> (ThreadId -> Occurred a -> b) -> React s (Singleton a) b
await' r f = Await (singleton r) >>>= \o -> do
	ti <- pure =<<< GetThreadId
	pure . f ti $ extract o

never :: React s es a
never = Never >>>= pure
