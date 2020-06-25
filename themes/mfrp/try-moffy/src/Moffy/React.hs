{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.React where

import Data.Kind
import Data.Type.Set
import Data.OneOrMore
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

await :: a -> (Occurred a -> b) -> React s (Singleton a) b
await r f = Await (singleton r) >>>= (pure . f . extract)
