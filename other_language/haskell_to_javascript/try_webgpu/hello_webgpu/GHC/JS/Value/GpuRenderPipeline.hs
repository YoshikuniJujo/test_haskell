module GHC.JS.Value.GpuRenderPipeline where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value.GpuDepthStencilObject qualified as JS.GpuDepthStencilObject
import GHC.JS.Value.GpuFragmentObject qualified as JS.GpuFragmentObject
import GHC.JS.Value.GpuVertexObject qualified as JS.GpuVertexObject

import GHC.JS.Value.GpuPipelineLayout qualified as JS.GpuPipelineLayout

-- createRenderPipeline :: G -> Descriptor -> IO

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
