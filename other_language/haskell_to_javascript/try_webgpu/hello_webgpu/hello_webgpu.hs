{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Bits
import Data.Maybe

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
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
import GHC.JS.Value.GpuAdapterInfo qualified as JS.GpuAdapterInfo

import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext
import GHC.JS.Value.CanvasContext.Gpu qualified as JS.GpuCanvasContext
import GHC.JS.Value.GpuShaderModule qualified as JS.GpuShaderModule
import GHC.JS.Value.GpuDevice qualified as JS.GpuDevice

import GHC.JS.Value.Float32Array qualified as JS.Float32Array

import GHC.JS.Value.GpuBufferUsage qualified as JS.GpuBufferUsage
import GHC.JS.Value.GpuQueue qualified as JS.GpuQueue

import GHC.JS.Value.GpuVertexFormat qualified as JS.GpuVertexFormat
import GHC.JS.Value.GpuVertexBufferAttributeLayout qualified as
	JS.GpuVertexBufferAttributeLayout
import GHC.JS.Value.GpuVertexBufferLayout qualified as JS.GpuVertexBufferLayout

import GHC.JS.Value.GpuDepthStencilObject qualified as JS.GpuDepthStencilObject

main :: IO ()
main = do
	putStrLn "Hello"

	print $ JS.Navigator.n
	let	Just g = JS.Navigator.gpu JS.Navigator.n
	print g
	let	format = JS.Gpu.getPreferredCanvasFormat g
	print format
	a <- JS.Gpu.requestAdapter g
	print a
	let	i = JS.GpuAdapter.info a
	print i
	print $ JS.GpuAdapterInfo.architecture i
	device <- JS.GpuAdapter.requestDevice a
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
--	maybe (error "bad") (JS.Object.consoleLog . JS.Object.toO) ctx

	JS.Object.consoleLog $ JS.Object.toO canvas
	JS.Object.consoleLog $ JS.Object.toO a

	JS.Value.consoleLog $ JS.Value.toV "Hello"

	JS.Object.consoleLog $ JS.Object.toO ctx

	let	conf = (JS.GpuCanvasContext.configuration device
			. fromJust $ JS.GpuCanvasContext.fromString format) {
			JS.GpuCanvasContext.alphaMode =
				Just JS.GpuCanvasContext.AlphaModePremultiplied
			}
		
	maybe (error "bad") (`JS.GpuCanvasContext.configure` conf)
		$ JS.CanvasContext.fromC ctx

	JS.Object.consoleLog $ JS.Object.toO ctx


	shdrm <- JS.GpuDevice.createShaderModule device
		$ (JS.GpuDevice.shaderModuleDescriptor shaders) {
			JS.GpuDevice.shaderModuleDescriptorLabel = Just "GOOD SHADERS"
			}

	JS.Object.consoleLog $ JS.Object.toO shdrm

	JS.Value.consoleLog $ JS.Value.toV (123 :: Int)

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
	JS.Value.consoleLog $ JS.Value.toV bffr

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
		pipelineDescriptor = JS.GpuDevice.RenderPipelineDescriptor {
			}
	pure ()

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
