{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.JS.Value.Navigator qualified as JS.Navigator
import GHC.JS.Value.Navigator.Webgpu qualified as JS.Navigator
import GHC.JS.Value.Window qualified as JS.Window

import GHC.JS.Value.Gpu qualified as JS.Gpu
import GHC.JS.Value.GpuAdapter qualified as JS.GpuAdapter

main :: IO ()
main = do
	putStrLn "Hello"
	print $ JS.Window.document JS.Window.w
	print $ JS.Navigator.n
	let	Just g = JS.Navigator.gpu JS.Navigator.n
	print g
	print $ JS.Gpu.getPreferredCanvasFormat g
	a <- JS.Gpu.requestAdapter g
	print a
	print =<< JS.GpuAdapter.requestDevice a
