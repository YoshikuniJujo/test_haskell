{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React (
	-- * Type
	React, EvReqs, EvOccs, Request(..),
	Adjustable, Firstable, CollapsableOccurred,
	-- * Interpret
	interpretReact, interpretReactSt,
	-- * Handle
	Handle, Handle', HandleSt, HandleSt',
	-- * Combinator
	await, await', adjust, first ) where

import Data.Type.Set
import Data.UnionSet

import MonadicFrp.React.Internal
import MonadicFrp.ThreadId.Type

await :: a -> (Occurred a -> b) -> React (Singleton a) b
await r f = Await (singleton r) (\oc ti -> pure (f $ extract oc, ti))

await' :: a -> (ThreadId -> Occurred a -> b) -> React (Singleton a) b
await' r f = Await (singleton r) (\oc ti -> pure (f ti $ extract oc, ti))
