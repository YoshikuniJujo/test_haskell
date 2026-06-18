module GHC.JS.Value.GpuBufferUsage where

import GHC.JS.Prim(JSVal)
import GHC.JS.Value.Object qualified as JS.Object

gpuBufferUsage :: JS.Object.O
gpuBufferUsage = JS.Object.otherO js_gpuBufferUsage

foreign import javascript "(() => { return GPUBufferUsage })"
	js_gpuBufferUsage :: JSVal
