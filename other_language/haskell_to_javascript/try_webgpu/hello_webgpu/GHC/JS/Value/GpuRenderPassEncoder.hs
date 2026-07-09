{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuRenderPassEncoder where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuColorAttachmentObject qualified as
	JS.GpuColorAttachmentObject
import GHC.JS.Value.GpuRenderPipeline qualified as JS.GpuRenderPipeline
import GHC.JS.Value.GpuBuffer qualified as JS.GpuBuffer
import GHC.JS.Value.GpuCommandEncoder qualified as JS.GpuCommandEncoder

import Control.Monad.ST

newtype G = G JSVal

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

begin ::
	JS.GpuCommandEncoder.G -> Descriptor -> IO G
begin g = js_begin g . JS.Value.toJSVal

foreign import javascript "((g, d) => { return g.beginRenderPass(d) })"
	js_begin :: JS.GpuCommandEncoder.G -> JSVal -> IO G

data Descriptor = Descriptor {
	colorAttachments :: [JS.GpuColorAttachmentObject.G]
	}

instance JS.Value.IsJSVal Descriptor where
	toJSVal d = JS.Value.toJSVal $ runST
		$ JS.Object.freeze =<< descriptorToObject d

instance JS.Value.V Descriptor where
	toV = JS.Object.toValue; fromV = JS.Object.fromValue

descriptorToObject :: Descriptor -> ST s (JS.Object.ST s)
descriptorToObject d = do
	o <- JS.Object.new
	JS.Object.set o "colorAttachments" $ colorAttachments d
	pure o

setPipeline :: G -> JS.GpuRenderPipeline.G -> IO ()
setPipeline = js_setPipeline

foreign import javascript "((g, p) => { g.setPipeline(p) })"
	js_setPipeline :: G -> JS.GpuRenderPipeline.G -> IO ()

setVertexBuffer :: G -> Int -> JS.GpuBuffer.G -> IO ()
setVertexBuffer = js_setVertexBuffer

foreign import javascript "((g, s, b) => { g.setVertexBuffer(s, b) })"
	js_setVertexBuffer :: G -> Int -> JS.GpuBuffer.G -> IO ()

draw :: G -> Int -> IO ()
draw = js_draw

foreign import javascript "((g, vc) => { g.draw(vc) })"
	js_draw :: G -> Int -> IO ()

end :: G -> IO ()
end = js_end

foreign import javascript "((g) => { g.end() })" js_end :: G -> IO ()
