{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuShaderModule where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice

newtype G = G { unG :: JSVal }

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO G

create :: JS.GpuDevice.G -> Descriptor -> IO G
create g smd = do
	o <- descriptorToObject smd
	create' g o

create' :: JS.GpuDevice.G -> JS.Object.O -> IO G
create' (JS.GpuDevice.G g) (JS.Value.toJSVal -> as) =
	G <$> js_create g as

foreign import javascript "((g, as) => { return g.createShaderModule(as); })"
	js_create :: JSVal -> JSVal -> IO JSVal

descriptorToObject :: Descriptor -> IO JS.Object.O
descriptorToObject smd = do
	o <- JS.Object.new @JS.Object.IO
	JS.Object.set o "code" $ descriptorCode smd
	maybe (pure ()) (JS.Object.set o "label")
		$ descriptorLabel smd
	maybe (pure ()) (JS.Object.set o "hints")
		$ descriptorHints smd
	maybe (pure ()) (JS.Object.set o "sourceMap")
		$ descriptorSourceMap smd
	JS.Object.freeze o

data Descriptor = Descriptor {
	descriptorCode :: String,
	descriptorLabel :: Maybe String,
	descriptorHints :: Maybe JS.Object.O,
	descriptorSourceMap :: Maybe JS.Object.O }

descriptor :: String -> Descriptor
descriptor cd = Descriptor {
	descriptorCode = cd,
	descriptorLabel = Nothing,
	descriptorHints = Nothing,
	descriptorSourceMap = Nothing }
