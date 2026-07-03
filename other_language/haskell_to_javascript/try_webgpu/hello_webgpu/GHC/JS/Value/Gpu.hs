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

getPreferredCanvasFormat :: G -> IO PreferredCanvasFormat
getPreferredCanvasFormat (G g) = do
	str <- fromJSString <$> js_getPreferredCanvasFormat g
	pure case str of
		"rgba8unorm" -> Rgba8Unorm
		"bgra8unorm" -> Bgra8Unorm
		_ -> error "bad"

preferredCanvasFormatToString :: PreferredCanvasFormat -> String
preferredCanvasFormatToString = \case
	Rgba8Unorm -> "rgba8unorm"
	Bgra8Unorm -> "bgra8unorm"

data PreferredCanvasFormat = Rgba8Unorm | Bgra8Unorm deriving Show

preferredCanvasFormatToTextureFormat ::
	PreferredCanvasFormat -> JS.GpuTextureFormat.G
preferredCanvasFormatToTextureFormat = \case
	Rgba8Unorm -> JS.GpuTextureFormat.Rgba8Unorm
	Bgra8Unorm -> JS.GpuTextureFormat.Bgra8Unorm

foreign import javascript
	"((g) => { const r = g.getPreferredCanvasFormat(); return r; })"
	js_getPreferredCanvasFormat :: JSVal -> IO JSVal
