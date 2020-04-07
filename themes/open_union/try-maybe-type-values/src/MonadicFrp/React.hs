{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React (
	React, EvReqs, EvOccs, Request(..), Firstable, CollapsableOccurred,
	interpret, await, adjust, first ) where

import Data.Type.Set
import Data.UnionSet

import MonadicFrp.React.Internal

await :: a -> (Occurred a -> b) -> React (Singleton a) b
await r f = Await (singleton r) (pure . f . extract)
