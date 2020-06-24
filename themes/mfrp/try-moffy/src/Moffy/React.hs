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
	Await :: EvReqs es -> Rct es (EvOccs es)
type React s e a = Freer s FTCQueue (Taggable s) (Rct e) a

data ThreadId = ThreadId Natural Int deriving (Show, Eq)

rootThreadId :: ThreadId
rootThreadId = ThreadId 0 0

forkThreadId :: ThreadId -> (ThreadId, ThreadId)
forkThreadId (ThreadId n i) =
	(ThreadId n $ i + 1, ThreadId (n `setBit` i) $ i + 1)
