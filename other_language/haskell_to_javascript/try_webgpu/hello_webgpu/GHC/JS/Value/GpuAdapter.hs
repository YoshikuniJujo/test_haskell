{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuAdapter where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

import GHC.JS.Value.GpuAdapterInfo qualified as JS.GpuAdapterInfo
import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice

newtype G = G { unG :: JSVal }

instance Show G where
	show g' = "(" ++ JS.Object.toString (JS.Object.toO g') ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO G

requestDevice :: G -> IO JS.GpuDevice.G
requestDevice (G g) = JS.GpuDevice.G <$> js_requestDevice g

foreign import javascript interruptible
	"(async function (g, cont) { const d = await g.requestDevice(); cont(d) })"
	js_requestDevice :: JSVal -> IO JSVal

info :: G -> JS.GpuAdapterInfo.G
info (G g) = JS.GpuAdapterInfo.G $ js_info g

foreign import javascript "((g) => { return g.info })" js_info :: JSVal -> JSVal
