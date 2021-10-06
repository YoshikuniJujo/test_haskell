{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.ForeignPtr
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Char
import Data.Color
import Data.KeySym
import Data.CairoContext
import Data.IORef
import System.Environment

import Graphics.UI.GLUT hiding (mainLoop)
import qualified Graphics.UI.GLUT as Gl

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.Internal
import Graphics.Gdk.EventStructures
import Graphics.Gdk.GdkDrawingContext

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Try.Tools
import Fbo
import Lib

main :: IO ()
main = do
	wdt <- newIORef 0
	hgt <- newIORef 0
	void $ initializeGlut
	void $ createWindow "foo"
	(fb, cbo@(TextureObject cb)) <- initializeFbo 512 512
	reshapeCallback $= Just \(Size w h) ->
		writeIORef wdt w >> writeIORef hgt h
	displayCallback $= do
		drawFramebuffer fb 512 512
		drawTexture cbo wdt hgt
	print cb
	forkIO Gl.mainLoop

	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr
	gdkWindowSetTitle w "あいうえお"
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval <$> gdkEventKey k
			print k
			pure $ kv /= KeySym (fromIntegral $ ord 'q')
		GdkEventGdkMap _ -> True <$ do
			putStrLn "Mapped"
			display w fb cb
		GdkEventGdkFocusChange _ -> True <$ do
			putStrLn "Focused"
			display w fb cb
		GdkEventGdkAny e -> True <$ print e

display :: GdkWindow -> FramebufferObject -> GLuint -> IO ()
display w@(GdkWindow pw) fb cb = do
	r <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w r \ctx -> do
		print ctx
		cr@(CairoT fcr) <- gdkDrawingContextGetCairoContext ctx
		withForeignPtr fcr \pcr -> do
			cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
			cairoRectangle cr 64 64 128 128
			cairoFill cr
--			drawFramebuffer fb 256 256
			c_gdk_cairo_draw_from_gl pcr pw (fromIntegral cb) glTexture 1 0 0 512 512
--			c_gdk_cairo_draw_from_gl pcr pw (fromIntegral cb) 1234 1 0 0 512 512
			cairoRectangle cr 128 128 128 128
			cairoFill cr
