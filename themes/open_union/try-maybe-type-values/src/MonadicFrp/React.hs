{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React (
	-- * Type
	React, EvReqs, EvOccs, Request(..),
	Adjustable, Firstable, CollapsableOccurred,
	-- * Handle
	Handle, Handle', HandleSt, HandleSt',
	-- * Interpret
	interpretReact, interpretReactSt,
	-- * Combinator
	await, await', adjust, first ) where

import Data.Type.Set (Singleton)
import Data.UnionSet (extract, singleton)

import MonadicFrp.React.Internal (
	React(..), EvReqs, EvOccs, Request(..),
	Adjustable, Firstable, CollapsableOccurred,
	Handle, Handle', HandleSt, HandleSt',
	interpretReact, interpretReactSt, adjust, first )
import MonadicFrp.ThreadId.Type (ThreadId)

---------------------------------------------------------------------------

await :: a -> (Occurred a -> b) -> React (Singleton a) b
await r f = Await (singleton r) \oc ti -> pure (f $ extract oc, ti)

await' :: a -> (ThreadId -> Occurred a -> b) -> React (Singleton a) b
await' r f = Await (singleton r) \oc ti -> pure (f ti $ extract oc, ti)
