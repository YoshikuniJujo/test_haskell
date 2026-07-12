{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Float32Array qualified as JS.Float32Array
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Navigator qualified as JS.Navigator
import GHC.JS.Value.Navigator.Webgpu qualified as JS.Navigator
import GHC.JS.Value.Window qualified as JS.Window
import GHC.JS.Value.Document qualified as JS.Document
import GHC.JS.Value.Element qualified as JS.Element
import GHC.JS.Value.HtmlElement.Canvas qualified as JS.HtmlCanvasElement
import GHC.JS.Value.HtmlElement.Canvas.WebGpu qualified as JS.HtmlCanvasElement
import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext
import GHC.JS.Value.CanvasContext.Gpu qualified as JS.GpuCanvasContext

import GHC.JS.Value.Gpu qualified as JS.Gpu
import GHC.JS.Value.GpuAdapter qualified as JS.GpuAdapter
import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice
import GHC.JS.Value.GpuCommandEncoder qualified as JS.GpuCommandEncoder
import GHC.JS.Value.GpuRenderPassEncoder qualified as JS.GpuRenderPassEncoder
import GHC.JS.Value.GpuQueue qualified as JS.GpuQueue
import GHC.JS.Value.GpuBuffer qualified as JS.GpuBuffer
import GHC.JS.Value.GpuBufferUsage qualified as JS.GpuBufferUsage
import GHC.JS.Value.GpuRenderPipeline qualified as JS.GpuRenderPipeline
import GHC.JS.Value.GpuShaderModule qualified as JS.GpuShaderModule
import GHC.JS.Value.GpuFragmentObject qualified as JS.GpuFragmentObject
import GHC.JS.Value.GpuVertexObject qualified as JS.GpuVertexObject
import GHC.JS.Value.GpuVertexFormat qualified as JS.GpuVertexFormat
import GHC.JS.Value.GpuVertexBufferLayout qualified as JS.GpuVertexBufferLayout
import GHC.JS.Value.GpuVertexBufferAttributeLayout qualified as
	JS.GpuVertexBufferAttributeLayout

import GHC.JS.Value.GpuTexture qualified as JS.GpuTexture
import GHC.JS.Value.GpuTextureFormat qualified as JS.GpuTextureFormat
import GHC.JS.Value.GpuColorAttachmentObject qualified as JS.GpuColorAttachmentObject

import GHC.JS.Value.Node qualified as JS.Node

import Data.Bits
import Data.Maybe
import Data.UnionColor

main :: IO ()
main = do
	cvs <- initWindow
	gpu <- initVulkan

	dvc <- JS.GpuAdapter.requestDevice =<< JS.Gpu.requestAdapter gpu
	JS.EventTarget.addEventListenerSimple (fromJust $ JS.Value.cast dvc)
		"uncapturederror" JS.Value.consoleLog

	fmt <- JS.GpuTextureFormat.preferredCanvasToConfig
		<$> JS.Gpu.getPreferredCanvasFormat gpu

	sdm <- JS.GpuShaderModule.create dvc
		$ (JS.GpuShaderModule.descriptor shaders) {
			JS.GpuShaderModule.descriptorLabel = Just "MY SHADERS" }
	bffr <- JS.GpuBuffer.create dvc $ (JS.GpuBuffer.descriptor
		(JS.Float32Array.byteLength vertices)
		(JS.GpuBufferUsage.Vertex .|. JS.GpuBufferUsage.CopyDst)) {
		JS.GpuBuffer.descriptorLabel = Just "VERTEX BUFFER" }
	JS.GpuQueue.writeBuffer (JS.GpuDevice.queue dvc)
		bffr 0 vertices 0 (JS.Float32Array.length vertices)

	ctx <- maybeError "Cannot get WebGPU context"
		. (JS.CanvasContext.fromC =<<)
		=<< JS.HtmlCanvasElement.getContext cvs
			JS.HtmlCanvasElement.ContextTypeWebGpu

	JS.GpuCanvasContext.configure ctx
		$ (JS.GpuCanvasContext.configuration dvc fmt) {
		JS.GpuCanvasContext.alphaMode =
			Just JS.GpuCanvasContext.AlphaModePremultiplied }

	cmdEnc <- JS.GpuCommandEncoder.create dvc
	pssEnc <- JS.GpuRenderPassEncoder.begin cmdEnc . renderPassDescriptor
		=<< JS.GpuCanvasContext.getCurrentTexture ctx

	JS.GpuRenderPassEncoder.setPipeline pssEnc
		=<< JS.GpuRenderPipeline.create dvc (pipelineDescriptor sdm fmt)
	JS.GpuRenderPassEncoder.setVertexBuffer pssEnc 0 bffr
	JS.GpuRenderPassEncoder.draw pssEnc 3
	JS.GpuRenderPassEncoder.end pssEnc
	JS.GpuQueue.submit (JS.GpuDevice.queue dvc)
		. (: []) =<< JS.GpuCommandEncoder.finish cmdEnc

initWindow :: IO JS.HtmlCanvasElement.C
initWindow = do
	cvs <- maybeError "not canvas"
		. JS.Element.fromE =<< JS.Document.createElement doc "canvas"
	cvs <$ do
		JS.Node.appendChild
			(JS.Node.toN $ JS.Document.body doc) (JS.Node.toN cvs)
		JS.HtmlCanvasElement.set cvs (
			JS.HtmlCanvasElement.Width 800,
			JS.HtmlCanvasElement.Height 600 )
	where	doc = JS.Window.document JS.Window.w

initVulkan :: IO JS.Gpu.G
initVulkan = maybeError "WebGPU not supported" $ JS.Navigator.gpu JS.Navigator.n

shaders :: String
shaders = """
	struct VertexOut {
		@builtin(position) position : vec4f,
		@location(0) color : vec4f
	}

	@vertex
	fn vertex_main(
		@location(0) position: vec4f,
		@location(1) color: vec4f) -> VertexOut
	{
		var output : VertexOut;
		output.position = position;
		output.color = color;
		return output;
	}

	@fragment
	fn fragment_main(fragData: VertexOut) -> @location(0) vec4f
	{
		return fragData.color;
	}
	"""

vertices :: JS.Float32Array.F
vertices = JS.Float32Array.fromList $ concat
	[p [0, 0.6], red, p [-0.5, -0.6], green, p [0.5, -0.6], blue]
	where
	p = (++ [0, 1])
	red = [1, 0, 0, 1]; green = [0, 1, 0, 1]; blue = [0, 0, 1, 1]

pipelineDescriptor :: JS.GpuShaderModule.G ->
	JS.GpuTextureFormat.CanvasConfig -> JS.GpuRenderPipeline.Descriptor
pipelineDescriptor sdm fmt = JS.GpuRenderPipeline.Descriptor {
	JS.GpuRenderPipeline.descriptorDepthStencil = Nothing,
	JS.GpuRenderPipeline.descriptorFragment = Just JS.GpuFragmentObject.G {
		JS.GpuFragmentObject.constants = Nothing,
		JS.GpuFragmentObject.entryPoint = Just "fragment_main",
		JS.GpuFragmentObject.gModule = sdm,
		JS.GpuFragmentObject.targets = [frgTgt] },
	JS.GpuRenderPipeline.descriptorLabel = Just "foobar",
	JS.GpuRenderPipeline.descriptorLayout = JS.GpuRenderPipeline.Auto,
	JS.GpuRenderPipeline.descriptorVertex = JS.GpuVertexObject.G {
		JS.GpuVertexObject.constants = Nothing,
		JS.GpuVertexObject.entryPoint = Just "vertex_main",
		JS.GpuVertexObject.gModule = sdm,
		JS.GpuVertexObject.buffers = vertexBuffers } }
	where
	frgTgt = JS.GpuFragmentObject.Target {
		JS.GpuFragmentObject.blend = Nothing,
		JS.GpuFragmentObject.format =
			JS.GpuTextureFormat.fromCanvasConfig fmt,
		JS.GpuFragmentObject.writeMask = Nothing }

vertexBuffers :: [JS.GpuVertexBufferLayout.G]
vertexBuffers = [
	JS.GpuVertexBufferLayout.G {
		JS.GpuVertexBufferLayout.attributes = attrs,
		JS.GpuVertexBufferLayout.arrayStride = 32,
		JS.GpuVertexBufferLayout.stepMode =
			JS.GpuVertexBufferLayout.StepModeVertex } ]
	where attrs = [
		JS.GpuVertexBufferAttributeLayout.G {
			JS.GpuVertexBufferAttributeLayout.shaderLocation = 0,
			JS.GpuVertexBufferAttributeLayout.offset = 0,
			JS.GpuVertexBufferAttributeLayout.format =
				JS.GpuVertexFormat.GFloat32x4 },
		JS.GpuVertexBufferAttributeLayout.G {
			JS.GpuVertexBufferAttributeLayout.shaderLocation = 1,
			JS.GpuVertexBufferAttributeLayout.offset = 16,
			JS.GpuVertexBufferAttributeLayout.format =
				JS.GpuVertexFormat.GFloat32x4 } ]

renderPassDescriptor :: JS.GpuTexture.G -> JS.GpuRenderPassEncoder.Descriptor
renderPassDescriptor txtr = JS.GpuRenderPassEncoder.Descriptor {
	JS.GpuRenderPassEncoder.colorAttachments = [
		JS.GpuColorAttachmentObject.G {
			JS.GpuColorAttachmentObject.clearValue =
				rgbaDoubleRaw 0 0.5 1 1,
			JS.GpuColorAttachmentObject.loadOp =
				JS.GpuColorAttachmentObject.Clear,
			JS.GpuColorAttachmentObject.storeOp =
				JS.GpuColorAttachmentObject.Store,
			JS.GpuColorAttachmentObject.view = txtr } ] }

maybeError :: MonadFail m => String -> Maybe a -> m a
maybeError em = \case Nothing -> fail em; Just x -> pure x
