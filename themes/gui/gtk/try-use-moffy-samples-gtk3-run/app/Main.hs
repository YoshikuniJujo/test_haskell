{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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

main :: IO ()
main = do
	img <- newImageMut @Argb32Mut 16 16

	putMultiPixels img positions $ PixelArgb32Straight 255 255 255 0
	putMultiPixels img positions2 $ PixelArgb32Straight 255 0 0 0
	
	putStrLn "Slozsoft"
	join $ Gtk.init <$> getProgName <*> getArgs
	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_ab_bool w "delete-event" (\_ _ -> pure False) Null
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	da <- Gtk.DrawingArea.new
	Gtk.Container.add w da
	G.Signal.connect_self_cairo_ud da "draw" (drawFunction img) Null

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
