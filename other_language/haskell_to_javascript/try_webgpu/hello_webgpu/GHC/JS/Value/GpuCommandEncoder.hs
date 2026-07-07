{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuCommandEncoder where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice

newtype G = G JSVal

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

create :: JS.GpuDevice.G -> IO G
create = js_create

foreign import javascript "((d) => { return d.createCommandEncoder() })"
	js_create :: JS.GpuDevice.G -> IO G

data Descriptor
