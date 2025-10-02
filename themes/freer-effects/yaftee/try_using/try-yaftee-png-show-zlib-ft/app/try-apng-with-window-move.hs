{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Apng.Decode qualified as Apng
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.Concurrent
import Data.Foldable
import Data.Maybe
import Data.Map qualified as Map
import Data.ByteString.FingerTree qualified as BSF
import Data.Text qualified as T
import Data.Color
import Data.Png.Header
import Data.CairoContext
import Data.CairoImage.Internal
import Data.IORef
import System.IO
import System.Environment
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Pipe.Draw

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Rendering.Cairo

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

	txt <- newIORef "YoshJ"

	hh <- openFile fpi ReadMode
	Right hdr <- Eff.runM
		. Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		$ PipeBS.hGet (32 * 64) hh Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			Png.decodeHeader "foobar"
	hClose hh
	print hdr

	let	wdt = fromIntegral $ headerWidth hdr
		hgt = fromIntegral $ headerHeight hdr
		ilm = headerInterlaceMethod hdr
		(yss, xss) = case ilm of
			InterlaceMethodNon -> ([0 .. hgt - 1] `zip` repeat (1, 1), replicate hgt [0 .. wdt - 1])
			InterlaceMethodAdam7 -> error "not implemented"
	print (wdt, hgt, ilm)
	writeIORef txt . T.pack $ show (wdt, hgt, ilm)

	img <- newImageArgb32Mut (wdt * 12) (hgt * 12)
	print img

	join $ Gtk.init <$> getProgName <*> getArgs
	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_ab_bool w "delete-event" (\_ _ -> pure False) Null
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	fctls <- newIORef Map.empty
	fnref <- newIORef 0
	imgs <- newIORef Map.empty
	now <- newIORef 0
	da <- Gtk.DrawingArea.new
	Gtk.Container.add w da
	G.Signal.connect_self_cairo_ud da "draw" (drawFunction txt fnref fctls imgs now) Null

	forkIO $ do
		fix \f -> do
			threadDelay 100000
			fn <- readIORef fnref
			when (fn == 0) f
		forever do
			fn <- readIORef fnref
			n <- readIORef now
			let	n' = (n + 1) `mod` fn
			fctl <- (Map.! n') <$> readIORef fctls
			threadDelay $ (fromIntegral (Apng.fctlDelayNum fctl) * 1000000 `div` fromIntegral (Apng.fctlDelayDen fctl))
			writeIORef now n'
			(G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null)

	forkIO do
		h <- openFile fpi ReadMode
		ib <- PipeZ.cByteArrayMalloc (64 * 64)
		ob <- PipeZ.cByteArrayMalloc (64 * 64)

		Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
			. Fail.runExc id id . Apng.apngRun_ @"foobar" . Pipe.run
			. (`Except.catch` IO.print @String)
			. (`Except.catch` IO.print @Zlib.ReturnCode)
			. void $ PipeBS.hGet (32 * 64) h Pipe.=$=
				PipeT.convert BSF.fromStrict Pipe.=$=

				Apng.apngPipe "foobar" hdr ib ob Pipe.=$= do
					Apng.BodyNull <- Pipe.await
					Apng.FrameNumber fn <- State.getN "foobar"
--					Eff.effBase $ writeIORef fnref fn
					for_ [0 .. fn - 1] \n -> do
						Apng.BodyFctl fctl <- Pipe.await
						let	w = fromIntegral $ Apng.fctlWidth fctl
							h = fromIntegral $ Apng.fctlHeight fctl

						i <- Eff.effBase $ newImageArgb32Mut w h
						Eff.effBase $ updateMap imgs n i

						Eff.effBase $ updateMap fctls n fctl
						Eff.effBase $ writeIORef fnref (n + 1)
--						Eff.effBase $ writeIORef txt . T.pack $ show fctl
						IO.print fctl
						let	wdt = fromIntegral $ Apng.fctlWidth fctl
							hgt = fromIntegral $ Apng.fctlHeight fctl
							yss = [0 .. hgt - 1] `zip` repeat (1, 1)
							xss = replicate hgt [0 .. wdt - 1]
						IO.print (wdt, hgt)
						PipeT.convert (\(Apng.BodyRgba r) -> r) Pipe.=$= drawColor' i yss xss
							(G.idleAdd (\_ -> Gtk.Widget.queueDraw da >> pure False) Null)
						Apng.BodyFdatEnd <- Pipe.await
						pure ()
		
		hClose h

	Gtk.Widget.showAll w
	Gtk.main

sampleLayout cr sz txt = do
	(l, d) <- (,) <$> pangoCairoCreateLayout cr <*> pangoFontDescriptionNew
	d `pangoFontDescriptionSet` Family "sans-serif"
	d `pangoFontDescriptionSet` AbsoluteSize sz
	d' <- pangoFontDescriptionFreeze d
	l `pangoLayoutSet` pangoFontDescriptionToNullable (Just d')
	l `pangoLayoutSet` (txt :: T.Text)
	l' <- pangoLayoutFreeze l
	pure l'

drawFunction :: IORef T.Text -> IORef Int -> IORef (Map.Map Int Apng.Fctl) ->
	IORef (Map.Map Int (Argb32Mut RealWorld)) -> IORef Int -> Gtk.DrawingArea.D -> CairoT r RealWorld -> Null -> IO Bool
-- drawFunction txt fnref fctls (CairoImageMutArgb32 -> img) _ cr Null = do
drawFunction txt fnref fctls imgs now _ cr Null = do
	n <- readIORef now
	img <- CairoImageMutArgb32 . (Map.! n) <$> readIORef imgs
	fctl <- (Map.! n) <$> readIORef fctls
	sfc <- CairoSurfaceTImage <$> cairoImageSurfaceCreateForCairoImageMut img
	ptn <- cairoPatternCreateForSurface sfc

	cairoTranslate cr
		(50 + fromIntegral (Apng.fctlXOffset fctl))
		(50 + fromIntegral (Apng.fctlYOffset fctl))
	cairoSetSource cr ptn
	cairoPaint cr
	cairoIdentityMatrix cr

	cairoMoveTo cr 400 30
	l <- sampleLayout cr 30 =<< readIORef txt
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	pangoCairoShowLayout cr l

	fn <- readIORef fnref

	cairoMoveTo cr 400 70
	l <- sampleLayout cr 30 . T.pack $ show fn ++ " " ++ show n
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	pangoCairoShowLayout cr l

	for_ [0 .. fn - 1] \n -> do
		cairoMoveTo cr 400 (100 + 20 * fromIntegral n)
		l <- sampleLayout cr 15 . T.pack . show . (Map.! n) =<< readIORef fctls
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
		pangoCairoShowLayout cr l

	pure False

updateMap :: Ord k => IORef (Map.Map k a) -> k -> a -> IO ()
updateMap m k v = modifyIORef m (Map.insert k v)
