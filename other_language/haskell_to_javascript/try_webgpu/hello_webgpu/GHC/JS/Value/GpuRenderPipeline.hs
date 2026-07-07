{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuRenderPipeline where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice
import GHC.JS.Value.GpuDepthStencilObject qualified as JS.GpuDepthStencilObject
import GHC.JS.Value.GpuFragmentObject qualified as JS.GpuFragmentObject
import GHC.JS.Value.GpuVertexObject qualified as JS.GpuVertexObject
import GHC.JS.Value.GpuPipelineLayout qualified as JS.GpuPipelineLayout

import Control.Monad.ST

newtype G = G JSVal

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

create :: JS.GpuDevice.G -> Descriptor -> IO G
create (JS.Value.toJSVal -> g) (JS.Value.toJSVal -> d) = G <$> js_create g d

foreign import javascript "((dvc, dsc) => { return dvc.createRenderPipeline(dsc) })"
	js_create :: JSVal -> JSVal -> IO JSVal

data Descriptor = Descriptor {
	descriptorDepthStencil ::
		Maybe JS.GpuDepthStencilObject.G,
	descriptorFragment :: Maybe JS.GpuFragmentObject.G,
	descriptorLabel :: Maybe String,
	descriptorLayout :: Layout,
	descriptorVertex :: JS.GpuVertexObject.G }

instance JS.Value.IsJSVal Descriptor where
	toJSVal d = JS.Value.toJSVal
		$ runST $ JS.Object.freeze =<< descriptorToObject d

instance JS.Value.V Descriptor where
	toV = JS.Object.toValue; fromV = JS.Object.fromValue

data Layout = Auto | Explicit JS.GpuPipelineLayout.G deriving Show

instance JS.Value.IsJSVal Layout where
	toJSVal = \case
		Auto -> JS.Value.toJSVal "auto"
		_ -> error "yet"

instance JS.Value.V Layout

descriptorToObject :: (Monad m, JS.Object.M o m) => Descriptor -> m o
descriptorToObject d = do
	o <- JS.Object.new
	maybe (pure ()) (JS.Object.set o "depthStencil")
		$ descriptorDepthStencil d
	maybe (pure ()) (JS.Object.set o "fragment")
		$ descriptorFragment d
	maybe (pure ()) (JS.Object.set o "label")
		$ descriptorLabel d
	JS.Object.set o "layout" $ descriptorLayout d
	JS.Object.set o "vertex" $ descriptorVertex d
	pure o
