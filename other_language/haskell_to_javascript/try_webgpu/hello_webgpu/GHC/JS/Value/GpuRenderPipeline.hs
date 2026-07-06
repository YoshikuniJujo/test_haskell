{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuRenderPipeline where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuDepthStencilObject qualified as JS.GpuDepthStencilObject
import GHC.JS.Value.GpuFragmentObject qualified as JS.GpuFragmentObject
import GHC.JS.Value.GpuVertexObject qualified as JS.GpuVertexObject
import GHC.JS.Value.GpuPipelineLayout qualified as JS.GpuPipelineLayout

import Control.Monad.ST

-- create :: G -> Descriptor -> IO

foreign import javascript "((dvc, dsc) => { return dvc.createRenderPipeline(dsc) })"
	js_create :: JSVal -> JSVal -> IO JSVal

data Descriptor = Descriptor {
	descriptorDepthStencil ::
		Maybe JS.GpuDepthStencilObject.G,
	descriptorFragment :: Maybe JS.GpuFragmentObject.G,
	descriptorLabel :: Maybe String,
	descriptorLayout :: Layout,
	descriptorVertex :: JS.GpuVertexObject.G
	}

data Layout = Auto | Explicit JS.GpuPipelineLayout.G deriving Show

-- instance JS.Value.IsJSVal where

descriptorToObject :: Descriptor -> ST s (JS.Object.ST s)
descriptorToObject d = do
	o <- JS.Object.new
--	maybe (pure ()) (JS.Object.set o "depthStencil")
--		$ descriptorDepthStencil d
	pure o
