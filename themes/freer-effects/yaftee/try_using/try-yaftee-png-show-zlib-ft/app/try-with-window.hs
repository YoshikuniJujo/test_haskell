{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.Concurrent
import Data.ByteString.FingerTree qualified as BSF
import Data.Color
import Data.Png.Header
import Data.CairoContext
import Data.CairoImage.Internal
import System.IO
import System.Environment
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Pipe.Draw

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Stopgap.Data.Ptr
import Stopgap.System.GLib qualified as G
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Container qualified as Gtk.Container
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea

import Lib

main :: IO ()
main = do
	fpi : opts <- getArgs

	let	blk = opts == ["--block"]

	hh <- openFile fpi ReadMode
	Right hdr <- Eff.runM
		. Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		$ PipeBS.hGet 32 hh Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			Png.decodeHeader "foobar"
	hClose hh
	print hdr

	let	wdt = fromIntegral $ headerWidth hdr
		hgt = fromIntegral $ headerHeight hdr
		ilm = headerInterlaceMethod hdr
		(yss, xss) = case ilm of
			InterlaceMethodNon -> ([0 ..] `zip` repeat (1, 1), repeat [0 ..])
			InterlaceMethodAdam7 -> (mkyss blk hgt ++ [(0, (1, 1))], mkxss wdt hgt ++ [[]])
	print (wdt, hgt, ilm)

	img <- newImageArgb32Mut wdt hgt
	print img

	join $ Gtk.init <$> getProgName <*> getArgs
	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_ab_bool w "delete-event" (\_ _ -> pure False) Null
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	da <- Gtk.DrawingArea.new
	Gtk.Container.add w da
	G.Signal.connect_self_cairo_ud da "draw" (drawFunction img) Null

	forkIO do
		h <- openFile fpi ReadMode
		ib <- PipeZ.cByteArrayMalloc 64
		ob <- PipeZ.cByteArrayMalloc 64

		Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
			. Fail.runExc id . Png.run_ @"foobar" . Pipe.run
			. (`Except.catch` IO.print @String)
			. (`Except.catch` IO.print @Zlib.ReturnCode)
			. void $ PipeBS.hGet 32 h Pipe.=$=
				PipeT.convert BSF.fromStrict Pipe.=$=
				Png.decode @Double "foobar" IO ib ob Pipe.=$=
--					(\_ -> pure ()) (\_ -> pure ()) Pipe.=$=
				PipeT.convert (either
					((`toRgba` AlphaWord8 255) <$>)
					id) Pipe.=$= do
				drawColor' img yss xss
					$ G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null
		hClose h

	Gtk.Widget.showAll w
	Gtk.main

drawFunction :: Argb32Mut RealWorld -> Gtk.DrawingArea.D -> CairoT r RealWorld -> Null -> IO Bool
drawFunction (CairoImageMutArgb32 -> img) _ cr Null = do
	sfc <- CairoSurfaceTImage <$> cairoImageSurfaceCreateForCairoImageMut img
	ptn <- cairoPatternCreateForSurface sfc
	cairoSetSource cr ptn
	cairoPaint cr
	pure False
