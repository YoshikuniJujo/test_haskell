{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React (
	React, EvReqs, EvOccs, Request(..), Firstable, CollapsableOccurred,
	interpretReact, interpretReactSt, await, await', adjust, first,
	Handle, Handle' ) where

import Data.Type.Set
import Data.UnionSet

import MonadicFrp.React.Internal
import MonadicFrp.ThreadId

await :: a -> (Occurred a -> b) -> React (Singleton a) b
await r f = Await (singleton r) (\oc ti -> pure (f $ extract oc, ti))

await' :: a -> (ThreadId -> Occurred a -> b) -> React (Singleton a) b
await' r f = Await (singleton r) (\oc ti -> pure (f ti $ extract oc, ti))
