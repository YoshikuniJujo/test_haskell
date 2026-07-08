{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Navigator qualified as JS.Navigator
import GHC.JS.Value.Navigator.Webgpu qualified as JS.Navigator
import GHC.JS.Value.Window qualified as JS.Window
import GHC.JS.Value.Document qualified as JS.Document
import GHC.JS.Value.Element qualified as JS.Element
import GHC.JS.Value.HtmlCollection qualified as JS.HtmlCollection
import GHC.JS.Value.HtmlElement.Canvas qualified as JS.HtmlCanvasElement
import GHC.JS.Value.HtmlElement.Canvas.WebGpu qualified as JS.HtmlCanvasElement

import GHC.JS.Value.Gpu qualified as JS.Gpu
import GHC.JS.Value.GpuAdapter qualified as JS.GpuAdapter

import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext
import GHC.JS.Value.CanvasContext.Gpu qualified as JS.GpuCanvasContext
import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice

import GHC.JS.Value.Float32Array qualified as JS.Float32Array

import GHC.JS.Value.GpuBufferUsage qualified as JS.GpuBufferUsage
import GHC.JS.Value.GpuQueue qualified as JS.GpuQueue

import GHC.JS.Value.GpuVertexFormat qualified as JS.GpuVertexFormat
import GHC.JS.Value.GpuVertexBufferAttributeLayout qualified as
	JS.GpuVertexBufferAttributeLayout
import GHC.JS.Value.GpuVertexBufferLayout qualified as JS.GpuVertexBufferLayout
import GHC.JS.Value.GpuVertexObject qualified as JS.GpuVertexObject

import GHC.JS.Value.GpuOverridableConstant qualified as JS.GpuOverridableConstant
import GHC.JS.Value.GpuBlendComponent qualified as JS.GpuBlendComponent

import GHC.JS.Value.GpuFragmentObject qualified as JS.GpuFragmentObject
import GHC.JS.Value.GpuTextureFormat qualified as JS.GpuTextureFormat

import GHC.JS.Value.GpuRenderPipeline qualified as JS.GpuRenderPipeline

import GHC.JS.Value.GpuCommandEncoder qualified as JS.GpuCommandEncoder
import GHC.JS.Value.GpuRenderPassEncoder qualified as JS.GpuRenderPassEncoder
import GHC.JS.Value.GpuColorAttachmentObject qualified as JS.GpuColorAttachmentObject

import Data.Bits
import Data.Maybe
import Data.UnionColor

main :: IO ()
main = do
	g <- maybeError "WebGPU not supported" $ JS.Navigator.gpu JS.Navigator.n
	a <- JS.Gpu.requestAdapter g
	print a
	device <- JS.GpuAdapter.requestDevice a
	JS.EventTarget.addEventListenerSimple
		(fromJust $ JS.Value.cast device) "uncapturederror" \ev -> do
		JS.Value.consoleLog "error error error"
		JS.Value.consoleLog ev
		putStrLn "error occur"
	print device

	let document = JS.Window.document JS.Window.w
	print document

	canvas <- JS.Document.getElementByTagName document "canvas"
	canvasList <- JS.Document.getElementsByTagName document "canvas"
--	canvas <- JS.Document.getElementsByTagName document "p"
--	canvas <- JS.Document.getElementById document "foobar"

	print canvas
	print canvasList
	print =<< JS.HtmlCollection.length canvasList
	cvs <- JS.HtmlCollection.item canvasList 0
	print cvs

	let	Just c = JS.Element.fromE cvs
	print =<< JS.HtmlCanvasElement.getWidth c
	print =<< JS.HtmlCanvasElement.getHeight c
	Just ctx <- JS.HtmlCanvasElement.getContext c
		JS.HtmlCanvasElement.ContextTypeWebGpu
	print ctx
	JS.Value.consoleLog ctx

	JS.Value.consoleLog canvas
	JS.Value.consoleLog a

	JS.Value.consoleLog "Hello"

	JS.Value.consoleLog ctx

	format <- JS.Gpu.getPreferredCanvasFormat g
	let	conf = (JS.GpuCanvasContext.configuration device
			$ JS.GpuTextureFormat.preferredCanvasToConfig format) {
			JS.GpuCanvasContext.alphaMode =
				Just JS.GpuCanvasContext.AlphaModePremultiplied
			}
		
	maybe (error "bad") (`JS.GpuCanvasContext.configure` conf)
		$ JS.CanvasContext.fromC ctx

	JS.Value.consoleLog ctx


	shdrm <- JS.GpuDevice.createShaderModule device
		$ (JS.GpuDevice.shaderModuleDescriptor shaders) {
			JS.GpuDevice.shaderModuleDescriptorLabel = Just "GOOD SHADERS"
			}

	JS.Value.consoleLog shdrm

	JS.Value.consoleLog (123 :: Int)

	print =<< JS.Float32Array.new (JS.Value.toV (10 :: Int))

	vertices <- JS.Float32Array.fromFloatList [
		0, 0.6, 0, 1, 1, 0,
		0, 1, -0.5, -0.6,  0, 1,
		0, 1, 0, 1, 0.5, -0.6,
		0, 1, 0, 0, 1, 1 ]
	print vertices
	print $ JS.Float32Array.byteLength vertices

	print JS.GpuBufferUsage.copySrc
	print JS.GpuBufferUsage.copyDst
	print JS.GpuBufferUsage.index
	print JS.GpuBufferUsage.mapRead
	print JS.GpuBufferUsage.queryResolve
	print JS.GpuBufferUsage.vertex

	bffr <- JS.GpuDevice.createBuffer device
		$ (JS.GpuDevice.bufferDescriptor
			(JS.Float32Array.byteLength vertices)
			(	JS.GpuBufferUsage.Vertex .|.
				JS.GpuBufferUsage.CopyDst )) {
			JS.GpuDevice.bufferDescriptorLabel =
				Just "VERTEX BUFFER" }
	print bffr
	JS.Value.consoleLog bffr

	let	queue = JS.GpuDevice.queue device
		bffrln = JS.Float32Array.length vertices
	print bffr
	print queue
	print bffrln
	JS.GpuQueue.writeBuffer queue bffr 0 vertices 0 bffrln

	let	attrs = [
			JS.GpuVertexBufferAttributeLayout.G {
				JS.GpuVertexBufferAttributeLayout.shaderLocation = 0,
				JS.GpuVertexBufferAttributeLayout.offset = 0,
				JS.GpuVertexBufferAttributeLayout.format =
					JS.GpuVertexFormat.GFloat32x4
				},
			JS.GpuVertexBufferAttributeLayout.G {
				JS.GpuVertexBufferAttributeLayout.shaderLocation = 1,
				JS.GpuVertexBufferAttributeLayout.offset = 16,
				JS.GpuVertexBufferAttributeLayout.format =
					JS.GpuVertexFormat.GFloat32x4
				}
			]
		vertexBuffers = [
			JS.GpuVertexBufferLayout.G {
				JS.GpuVertexBufferLayout.attributes = attrs,
				JS.GpuVertexBufferLayout.arrayStride = 32,
				JS.GpuVertexBufferLayout.stepMode =
					JS.GpuVertexBufferLayout.StepModeVertex
				}
			]
		pipelineDescriptor = JS.GpuRenderPipeline.Descriptor {
			JS.GpuRenderPipeline.descriptorDepthStencil =
				Nothing,
			JS.GpuRenderPipeline.descriptorFragment =
				Just JS.GpuFragmentObject.G {
					JS.GpuFragmentObject.constants = Nothing,
					JS.GpuFragmentObject.entryPoint =
						Just "fragment_main",
					JS.GpuFragmentObject.gModule = shdrm,
					JS.GpuFragmentObject.targets = [
						JS.GpuFragmentObject.Target {
							JS.GpuFragmentObject.blend = Nothing,
							JS.GpuFragmentObject.format = JS.GpuTextureFormat.fromPreferredCanvas format,
							JS.GpuFragmentObject.writeMask = Nothing
							} ]
					},
			JS.GpuRenderPipeline.descriptorLabel = Just "foobar",
			JS.GpuRenderPipeline.descriptorLayout = JS.GpuRenderPipeline.Auto,
			JS.GpuRenderPipeline.descriptorVertex =
				JS.GpuVertexObject.G {
					JS.GpuVertexObject.constants = Nothing,
					JS.GpuVertexObject.entryPoint =
						Just "vertex_main",
					JS.GpuVertexObject.gModule = shdrm,
					JS.GpuVertexObject.buffers = vertexBuffers }
			}
	JS.Value.consoleLog attrs
	JS.Value.consoleLog vertexBuffers
	JS.Value.js_consoleLog . JS.GpuOverridableConstant.fromValue
		$ JS.GpuOverridableConstant.I32 (- 5)
	JS.Value.consoleLog JS.GpuBlendComponent.def
	print JS.GpuFragmentObject.js_red
	print JS.GpuFragmentObject.js_green
	print JS.GpuFragmentObject.js_blue
	print JS.GpuFragmentObject.js_alpha
	print JS.GpuFragmentObject.js_all
	JS.Value.consoleLog $ [
		JS.GpuFragmentObject.Target {
			JS.GpuFragmentObject.blend = Nothing,
			JS.GpuFragmentObject.format =
				JS.GpuTextureFormat.R8Unorm,
			JS.GpuFragmentObject.writeMask = Nothing } ]
	print =<< JS.Gpu.getPreferredCanvasFormat g
	JS.Value.consoleLog pipelineDescriptor
	renderPipeline <- JS.GpuRenderPipeline.create device pipelineDescriptor
	JS.Value.consoleLog renderPipeline
	commandEncoder <- JS.GpuCommandEncoder.create device
	JS.Value.consoleLog commandEncoder
	txtr <- maybe (error "bad") JS.GpuCanvasContext.getCurrentTexture
		$ JS.CanvasContext.fromC ctx
	JS.Value.consoleLog txtr
	let	renderPassDescriptor = JS.GpuRenderPassEncoder.Descriptor {
			JS.GpuRenderPassEncoder.colorAttachments = [
				JS.GpuColorAttachmentObject.G {
					JS.GpuColorAttachmentObject.clearValue =
						rgbaDoubleRaw 0 0.5 1 1,
					JS.GpuColorAttachmentObject.loadOp =
--						JS.GpuColorAttachmentObject.Load,
						JS.GpuColorAttachmentObject.Clear,
					JS.GpuColorAttachmentObject.storeOp =
						JS.GpuColorAttachmentObject.Store,
					JS.GpuColorAttachmentObject.view = txtr
					}
				]
			}
	JS.Value.consoleLog renderPassDescriptor
	passEncoder <- JS.GpuCommandEncoder.beginRenderPass
		commandEncoder renderPassDescriptor
	JS.Value.consoleLog renderPipeline
	JS.Value.consoleLog passEncoder
	JS.GpuRenderPassEncoder.setPipeline passEncoder renderPipeline
	JS.GpuRenderPassEncoder.setVertexBuffer passEncoder 0 bffr
	JS.GpuRenderPassEncoder.draw passEncoder 3
	JS.GpuRenderPassEncoder.end passEncoder
	cbffr <- JS.GpuCommandEncoder.finish commandEncoder
	JS.Value.consoleLog cbffr
	JS.GpuQueue.submit queue [cbffr]

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

maybeError :: MonadFail m => String -> Maybe a -> m a
maybeError em = \case Nothing -> fail em; Just x -> pure x
