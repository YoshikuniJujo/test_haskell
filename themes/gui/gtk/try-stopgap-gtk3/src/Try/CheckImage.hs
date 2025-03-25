{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.CheckImage (checkImage) where

import Control.Monad
import Control.Monad.ST
import System.Environment

import Data.Foldable
import Data.Vector.Mutable qualified as V
import Data.Word
import Data.CairoContext
import Data.CairoImage
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.CairoSurfaceT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Surfaces.PngSupport

import Stopgap.Data.Ptr
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Container qualified as Gtk.Container
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea

checkImage :: Int -> Int -> FilePath -> V.MVector RealWorld Word8 -> IO ()
checkImage w h fp v = do
	join $ Gtk.init <$> getProgName <*> getArgs
	win <- Gtk.Window.new  Gtk.Window.Toplevel
	G.Signal.connect_ab_bool win "delete-event" (\_ _ -> pure False) Null
	G.Signal.connect_void_void win "destroy" Gtk.mainQuit Null

	da <- Gtk.DrawingArea.new
	Gtk.Container.add win da

	pngsfc <- cairoSurfaceCreateFromPng fp

	cimgm <- newImageMut @Argb32Mut (fromIntegral w) (fromIntegral h)
	rgbaVectorToArgbImage w h v cimgm
	cimg <- cairoImageFreeze $ CairoImageMutArgb32 cimgm
	imgsfc <- cairoImageSurfaceCreateForCairoImage cimg

	G.Signal.connect_self_cairo_ud da "draw" (drawFunction w h pngsfc imgsfc) Null

	Gtk.Widget.showAll win
	Gtk.main

rgbaVectorToArgbImage :: Int -> Int -> V.MVector RealWorld Word8 -> Argb32Mut RealWorld -> IO ()
rgbaVectorToArgbImage w h v img = for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
	[r, g, b, a] <- (\i -> V.read v ((y * w + x) * 4 + i)) `mapM` [0 .. 3]
	putPixel img (fromIntegral x) (fromIntegral y) $ PixelArgb32Straight a r g b

drawFunction :: (IsCairoSurfaceT sr, IsCairoSurfaceT sr') =>
	Int -> Int -> sr s RealWorld -> sr' s' RealWorld ->
	Gtk.DrawingArea.D -> CairoT r RealWorld -> Null -> IO Bool
drawFunction w _ sr sr' _ cr Null = do
	putStrLn "DRAW FUNCTION"
	cairoSetSourceSurface cr sr 100 100
	cairoPaint cr
	cairoSetSourceSurface cr sr' (100 + fromIntegral w * 2) 100
	cairoPaint cr
	cairoMoveTo cr 100 100
	cairoLineTo cr 300 300
	cairoStroke cr
	pure False
