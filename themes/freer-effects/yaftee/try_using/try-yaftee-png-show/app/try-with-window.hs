{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.CairoImage.Internal
import System.Environment

import Data.CairoContext
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT

import Stopgap.Data.Ptr
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Container qualified as Gtk.Container
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea

import Control.Concurrent

import Stopgap.System.GLib qualified as G

import System.Environment

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import System.IO
import Data.Png.Header
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png

import Control.Concurrent

import Graphics.Pipe.Draw

import Pipe.Huffman qualified as Huffman

main :: IO ()
main = do
	fpi : opts <- getArgs

	let	block = opts == ["--block"]

	hh <- openFile fpi ReadMode

	(_, hdr) <- Eff.runM . (`State.run` header0)
		. Except.run @String . Fail.runExc id id . Png.run @"deflate" . Pipe.run
		$ PipeBS.hGet (64 * 64) hh Pipe.=$= Png.decodeHeader "deflate" \hdr -> do
			IO.print hdr
			State.put hdr

	hClose hh

	let	(fromIntegral -> wdt, fromIntegral -> hgt) = (headerWidth hdr, headerHeight hdr)
		ct = headerColorType hdr
		itl = headerInterlaceMethod hdr

	print itl

	img <- newImageMut @Argb32Mut wdt hgt
	
	join $ Gtk.init <$> getProgName <*> getArgs
	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_ab_bool w "delete-event" (\_ _ -> pure False) Null
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	da <- Gtk.DrawingArea.new
	Gtk.Container.add w da
	G.Signal.connect_self_cairo_ud da "draw" (drawFunction img) Null

	forkIO do
		h <- openFile fpi ReadMode

		case (ct, itl) of
			(ColorTypeColorAlpha, InterlaceMethodNon) ->

				Eff.runM . Except.run @String . Fail.runExc id id . Png.run @"deflate"

					. (`State.run` Huffman.PhaseOthers)
					. (`State.run` Huffman.IsLiteral @Int (const False))

					. Pipe.run $
					PipeBS.hGet (64 * 64) h Pipe.=$=
					(void (Png.decode "deflate" IO.print) `Except.catch` IO.print @String) Pipe.=$=
					(drawCairoImageRgba32 IO img wdt hgt . void . Eff.effBase $
						G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null)

			(ColorTypeColorAlpha, InterlaceMethodAdam7) ->

				Eff.runM . Except.run @String . Fail.runExc id id . Png.run @"deflate"

					. (`State.run` Huffman.PhaseOthers)
					. (`State.run` Huffman.IsLiteral @Int (const False))

					. Pipe.run $
					PipeBS.hGet (64 * 64) h Pipe.=$=
					(void (Png.decode "deflate" IO.print) `Except.catch` IO.print @String) Pipe.=$=
					(drawCairoImageRgba32Adam7 IO block img wdt hgt . void . Eff.effBase $
						G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null)

			(ColorTypeColor, InterlaceMethodNon) ->

				Eff.runM . Except.run @String . Fail.runExc id id . Png.run @"deflate"

					. (`State.run` Huffman.PhaseOthers)
					. (`State.run` Huffman.IsLiteral @Int (const False))

					. Pipe.run $
					PipeBS.hGet (64 * 64) h Pipe.=$=
					(void (Png.decode "deflate" IO.print) `Except.catch` IO.print @String) Pipe.=$=
					(drawCairoImageRgb24 IO img wdt hgt . void . Eff.effBase $
						G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null)

			(ColorTypeColor, InterlaceMethodAdam7) ->

				Eff.runM . Except.run @String . Fail.runExc id id . Png.run @"deflate"

					. (`State.run` Huffman.PhaseOthers)
					. (`State.run` Huffman.IsLiteral @Int (const False))

					. Pipe.run $
					PipeBS.hGet (64 * 64) h Pipe.=$=
					(void (Png.decode "deflate" IO.print) `Except.catch` IO.print @String) Pipe.=$=
					(drawCairoImageRgb24Adam7 IO block img wdt hgt . void . Eff.effBase $
						G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null)

		hClose h

{-
	forkIO do

		putMultiPixels img positions $ PixelArgb32Straight 255 255 255 0

		G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null

		threadDelay 1000000

		putMultiPixels img positions2 $ PixelArgb32Straight 255 0 0 0

		G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null

		pure ()
		-}

	Gtk.Widget.showAll w
	Gtk.main

drawFunction :: Argb32Mut RealWorld -> Gtk.DrawingArea.D -> CairoT r RealWorld -> Null -> IO Bool
drawFunction (CairoImageMutArgb32 -> img) _ cr Null = do

	sfc <- CairoSurfaceTImage <$> cairoImageSurfaceCreateForCairoImageMut img
	ptn <- cairoPatternCreateForSurface sfc
	cairoSetSource cr ptn
	cairoPaint cr

	pure False

positions = [ (x, y) | x <- [3 .. 10], y <- [3 .. 10] ]

positions2 = [ (x, y) | x <- [2 .. 11], y <- [2 .. 11], x == 2 || x == 11 || y == 2 || y == 11 ]

putMultiPixels _ [] _ = pure ()
putMultiPixels img ((x, y) : xys) c = putPixel img x y c >> putMultiPixels img xys c
