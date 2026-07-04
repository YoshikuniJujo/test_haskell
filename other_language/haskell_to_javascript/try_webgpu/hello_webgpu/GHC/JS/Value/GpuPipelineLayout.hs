{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuPipelineLayout where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

-- yet

data G
	deriving Show

instance JS.Value.IsJSVal G where toJSVal = undefined
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue
