{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React (
	React, EvReqs, EvOccs, Request(..), Mergeable, CollapseOccurred,
	Or(..),
	await, interpret, adjust, first, first'
	) where

import MonadicFrp.React.Internal

await :: EvReqs es -> (EvOccs es -> React es a) -> React es a
await = Await
