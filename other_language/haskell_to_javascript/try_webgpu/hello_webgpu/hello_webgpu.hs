{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

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

main :: IO ()
main = do
	putStrLn "Hello"

	print $ JS.Navigator.n
	let	Just g = JS.Navigator.gpu JS.Navigator.n
	print g
	print $ JS.Gpu.getPreferredCanvasFormat g
	a <- JS.Gpu.requestAdapter g
	print a
	let	i = JS.GpuAdapter.info a
	print i
	print $ JS.GpuAdapterInfo.architecture i
	print =<< JS.GpuAdapter.requestDevice a

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
	ctx <- JS.HtmlCanvasElement.getContext c
		JS.HtmlCanvasElement.ContextTypeWebGpu
	print ctx
