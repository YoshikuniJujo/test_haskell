{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Gpu where

import GHC.JS.Prim (JSVal, fromJSString)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuAdapter qualified as JS.GpuAdapter

import GHC.JS.Value.GpuTextureFormat qualified as JS.GpuTextureFormat

newtype G = G { unG :: JSVal }

instance Show G where
	show g' = "(" ++ JS.Object.toString (JS.Object.toO g') ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO G

requestAdapter :: G -> IO JS.GpuAdapter.G
requestAdapter (G g) = JS.GpuAdapter.G <$> js_requestAdapter g

foreign import javascript interruptible
	"(async function (g, cont) { const a = await g.requestAdapter(); cont(a) })"
	js_requestAdapter :: JSVal -> IO JSVal

getPreferredCanvasFormat :: G -> IO JS.GpuTextureFormat.PreferredCanvas
getPreferredCanvasFormat (G g) = do
	str <- fromJSString <$> js_getPreferredCanvasFormat g
	pure case str of
		"rgba8unorm" -> JS.GpuTextureFormat.PreferredCanvasRgba8Unorm
		"bgra8unorm" -> JS.GpuTextureFormat.PreferredCanvasBgra8Unorm
		_ -> error "bad"

foreign import javascript
	"((g) => { const r = g.getPreferredCanvasFormat(); return r; })"
	js_getPreferredCanvasFormat :: JSVal -> IO JSVal
