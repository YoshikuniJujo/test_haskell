{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

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

	o <- JS.Object.new
	JS.Object.set o "device" $ JS.Value.toV device
	JS.Object.set o "format" $ JS.Value.toV format
	JS.Object.set o "alphaMode" $ JS.Value.toV "premultiplied"

	JS.Object.consoleLog o

	maybe (error "bad") (`JS.GpuCanvasContext.configure` o) $ JS.CanvasContext.fromC ctx

	JS.Object.consoleLog $ JS.Object.toO ctx

	shdr <- JS.Object.new
	JS.Object.set shdr "code" $ JS.Value.toV shaders
	JS.Object.set shdr "label" $ JS.Value.toV "SHADERS"

	shdrm <- JS.GpuDevice.createShaderModule device shdr
	JS.Object.consoleLog $ JS.Object.toO shdrm

	JS.Value.consoleLog $ JS.Value.toV (123 :: Int)

	print =<< JS.Float32Array.new (JS.Value.toV (10 :: Int))

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
