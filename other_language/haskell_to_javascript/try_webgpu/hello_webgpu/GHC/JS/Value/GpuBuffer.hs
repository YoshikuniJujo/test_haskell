{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuBuffer where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice
import GHC.JS.Value.GpuBufferUsage qualified as JS.GpuBufferUsage

newtype G = G { unG :: JSVal }

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO G

create :: JS.GpuDevice.G -> Descriptor -> IO G
create g d = create' g =<< descriptorToObject d

create' :: JS.GpuDevice.G -> JS.Object.O -> IO G
create' (JS.GpuDevice.G g) (JS.Value.toJSVal -> d) =
	G <$> js_create g d

foreign import javascript "((g, d) => { return g.createBuffer(d); })"
	js_create :: JSVal -> JSVal -> IO JSVal

descriptorToObject :: Descriptor -> IO JS.Object.O
descriptorToObject d = do
	o <- JS.Object.new @JS.Object.IO
	maybe (pure ()) (JS.Object.set o "label")
		$ descriptorLabel d
	maybe (pure ()) (JS.Object.set o "mappedAtCreation")
		$ descriptorMappedAtCreation d
	JS.Object.set o "size" $ descriptorSize d
	JS.Object.set o "usage" $ descriptorUsage d
	JS.Object.freeze o

data Descriptor = Descriptor {
	descriptorLabel :: Maybe String,
	descriptorMappedAtCreation :: Maybe Bool,
	descriptorSize :: Int,
	descriptorUsage :: JS.GpuBufferUsage.G }
	deriving Show

descriptor :: Int -> JS.GpuBufferUsage.G -> Descriptor
descriptor s u = Descriptor {
	descriptorLabel = Nothing,
	descriptorMappedAtCreation = Nothing,
	descriptorSize = s,
	descriptorUsage = u }
