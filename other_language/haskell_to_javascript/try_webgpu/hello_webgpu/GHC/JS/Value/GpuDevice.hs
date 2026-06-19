{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuDevice where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget

import GHC.JS.Value.GpuShaderModule qualified as JS.GpuShaderModule

import GHC.JS.Value.GpuBuffer qualified as JS.GpuBuffer
import GHC.JS.Value.GpuQueue qualified as JS.GpuQueue

newtype G = G JSVal

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v

instance JS.Value.V G where
	toV = JS.EventTarget.toValue; fromV = JS.EventTarget.fromValue

instance JS.Object.IsO G
instance JS.EventTarget.IsE G

createShaderModule' :: G -> JS.Object.O -> IO JS.GpuShaderModule.G
createShaderModule' (G g) (JS.Value.toJSVal -> as) =
	JS.GpuShaderModule.G <$> js_createShaderModule g as

foreign import javascript "((g, as) => { return g.createShaderModule(as); })"
	js_createShaderModule :: JSVal -> JSVal -> IO JSVal

createBuffer :: G -> JS.Object.O -> IO JS.GpuBuffer.G
createBuffer (G g) (JS.Value.toJSVal -> d) =
	JS.GpuBuffer.G <$> js_createBuffer g d

foreign import javascript "((g, d) => { return g.createBuffer(d); })"
	js_createBuffer :: JSVal -> JSVal -> IO JSVal

queue :: G -> JS.GpuQueue.G
queue (G g) = JS.GpuQueue.G $ js_queue g

foreign import javascript "((g) => { return g.queue; })"
	js_queue :: JSVal -> JSVal
