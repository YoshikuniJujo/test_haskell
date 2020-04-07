{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React (
	React, EvReqs, EvOccs, Request(..), First, CollapsableOccurred,
	Or(..),
	await_, interpret, adjust, first_, first
	) where

import MonadicFrp.React.Internal

await_ :: EvReqs es -> (EvOccs es -> React es a) -> React es a
await_ = Await
