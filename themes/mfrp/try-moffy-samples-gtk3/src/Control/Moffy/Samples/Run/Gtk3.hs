{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Run.Gtk3 (runSingleWin) where

import Control.Monad
import Control.Monad.ST
import Control.Concurrent
import Control.Concurrent.STM
import Data.Type.Set
import Data.OneOrMore qualified as OOM
import Data.OneOrMoreApp
import Data.Bits
import Data.Maybe
import Data.Text qualified as T
import Data.Color
import System.Environment

import Control.Moffy
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.CalcTextExtents
import Control.Moffy.Samples.View

import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Surfaces.PngSupport

import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Rendering.Cairo

import Stopgap.Data.Ptr
import Stopgap.System.GLib qualified as G
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Container qualified as Gtk.Container
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.Graphics.UI.Gdk.Event qualified as Gdk.Event
import Stopgap.Graphics.UI.Gdk.Event.Button qualified as Gdk.Event.Button
import Stopgap.Graphics.UI.Gdk.Event.Motion qualified as Gdk.Event.Motion

type Events = CalcTextExtents :-
	Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent

clicked :: TChan (EvOccs Events) ->
	Gtk.DrawingArea.D -> Gdk.Event.Button.B -> ud -> IO Bool
clicked ceo _da eb _ud = do
	case Gdk.Event.Button.bType eb of
		Gdk.Event.ButtonPress -> atomically . writeTChan ceo
			$ expand (Mouse.OccMove (mousePoint eb) >- Singleton (Mouse.OccDown $ mouseButton eb) :: EvOccs (Mouse.Move :- Singleton Mouse.Down))
		_ -> pure ()
	pure True

released :: TChan (EvOccs Events) ->
	Gtk.DrawingArea.D -> Gdk.Event.Button.B -> ud -> IO Bool
released ceo _da eb _ud = do
	case Gdk.Event.Button.bType eb of
		Gdk.Event.ButtonRelease -> atomically . writeTChan ceo
			$ expand (Mouse.OccMove (mousePoint eb) >- Singleton (Mouse.OccUp $ mouseButton eb) :: EvOccs (Mouse.Move :- Singleton Mouse.Up))
		_ -> pure ()
	pure True

moved :: TChan (EvOccs Events) ->
	Gtk.DrawingArea.D -> Gdk.Event.Motion.M -> ud -> IO Bool
moved ceo _da em _ud = do
	atomically . writeTChan ceo . expand . Singleton . Mouse.OccMove $ movePoint em
	pure True

mouseButton :: Gdk.Event.Button.B -> Mouse.Button
mouseButton eb = case Gdk.Event.Button.bButton eb of
	1 -> Mouse.ButtonPrimary
	2 -> Mouse.ButtonMiddle
	3 -> Mouse.ButtonSecondary
	_ -> Mouse.ButtonMiddle

mousePoint :: Gdk.Event.Button.B -> Point
mousePoint eb = (Gdk.Event.Button.bX eb, Gdk.Event.Button.bY eb)

movePoint :: Gdk.Event.Motion.M -> Point
movePoint em = (Gdk.Event.Motion.mX em, Gdk.Event.Motion.mY em)

runSingleWin ::
	TChan (EvReqs Events) -> TChan (EvOccs Events) -> TChan View -> IO ()
runSingleWin cer ceo cv = do
	crd <- atomically $ newTVar []
	cte <- atomically newTChan

	join $ Gtk.init <$> getProgName <*> getArgs

	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	da <- Gtk.DrawingArea.new
	Gtk.Container.add w da
	Gtk.Widget.addEvents da $
		Gdk.Event.ButtonPressMask .|.
		Gdk.Event.ButtonReleaseMask .|.
		Gdk.Event.ButtonMotionMask
	G.Signal.connect_self_button_ud
		da "button-press-event" (clicked ceo) Null
	G.Signal.connect_self_button_ud
		da "button-release-event" (released ceo) Null
	G.Signal.connect_self_motion_ud
		da "motion-notify-event" (moved ceo) Null
	G.Signal.connect_self_cairo_ud da "draw" (drawFunction crd ceo cte) Null

	Gtk.Widget.showAll w

	forkIO . forever $ atomically (readTChan cer) >>= \r -> do
		case OOM.project r of
			Nothing -> pure ()
			Just (CalcTextExtentsReq fn fs t) -> do
				atomically $ writeTChan cte (fn, fs, t)
				void $ G.idleAdd
					(\_ -> Gtk.Widget.queueDraw da >>
						pure False)
					Null

	forkIO . forever $ atomically (readTChan cv) >>= \case
		Stopped -> void $ G.idleAdd
			(\_ -> Gtk.mainQuit >> pure False) Null
		View v -> do
			atomically $ writeTVar crd v
			void $ G.idleAdd
				(\_ -> Gtk.Widget.queueDraw da >> pure False)
				Null
		v -> print v

	Gtk.main

drawFunction :: TVar [View1] ->
	TChan (EvOccs Events) -> TChan (FontName, FontSize, T.Text) ->
	Gtk.DrawingArea.D -> CairoT r RealWorld -> Null -> IO Bool
drawFunction crd ceo cte _ cr Null = do
	atomically (tryReadTChan cte) >>= \case
		Nothing -> pure ()
		Just (fn, fs, txt) -> occCalcTextExtents ceo cr fn fs txt
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoPaint cr
	(drawView1 cr `mapM_`) =<< atomically (readTVar crd)
	pure False

drawView1 :: CairoT r RealWorld -> View1 -> IO ()
drawView1 cr (Box
	(realToFrac -> l, realToFrac -> u)
	(realToFrac -> r, realToFrac -> d)
	(rgbRealToFrac -> clr)) = do
	cairoSetSourceRgb cr clr
	cairoRectangle cr l u (r - l) (d - u)
	cairoFill cr
drawView1 cr (VLine (rgbRealToFrac -> clr) lw
	(realToFrac -> l, realToFrac -> u)
	(realToFrac -> r, realToFrac -> d)) = do
	cairoSetSourceRgb cr clr
	cairoSetLineWidth cr $ realToFrac lw
	cairoMoveTo cr l u
	cairoLineTo cr r d
	cairoStroke cr
drawView1 cr (VText (rgbRealToFrac -> clr)
	fn (realToFrac -> fs) (realToFrac -> x, realToFrac -> y) txt) = do
	(l, d) <- (,) <$> pangoCairoCreateLayout cr <*> pangoFontDescriptionNew
	d `pangoFontDescriptionSet` Family fn
	d `pangoFontDescriptionSet` AbsoluteSize fs
	d' <- pangoFontDescriptionFreeze d
	l `pangoLayoutSet` pangoFontDescriptionToNullable (Just d')
	l `pangoLayoutSet` txt
	l' <- pangoLayoutFreeze l
	cairoMoveTo cr x y
	cairoSetSourceRgb cr clr
	pangoCairoShowLayout cr l'
drawView1 cr (VImage
	(realToFrac -> x, realToFrac -> y) w h dt) = do
	sfc <- cairoSurfaceCreateFromPngByteString dt
	w0 <- cairoImageSurfaceGetWidth sfc
	h0 <- cairoImageSurfaceGetHeight sfc
	cairoTranslate cr x y
	cairoScale cr
		(realToFrac w / fromIntegral w0)
		(realToFrac h / fromIntegral h0)
	cairoSetSourceSurface cr sfc 0 0
	cairoPaint cr

	cairoIdentityMatrix cr
drawView1 cr NotImplemented = putStrLn "NOT IMPLEMENTED"

occCalcTextExtents ::
	TChan (EvOccs (CalcTextExtents :- GuiEv)) -> CairoT r RealWorld -> String -> Double -> T.Text -> IO ()
occCalcTextExtents co cr fn fs txt = do
	(l, d) <- (,) <$> pangoCairoCreateLayout cr <*> pangoFontDescriptionNew
	d `pangoFontDescriptionSet` Family fn
	d `pangoFontDescriptionSet` AbsoluteSize (realToFrac fs)
	d' <- pangoFontDescriptionFreeze d
	l `pangoLayoutSet` pangoFontDescriptionToNullable (Just d')
	l `pangoLayoutSet` txt
	l' <- pangoLayoutFreeze l
	let	PixelExtents ie le = pangoLayoutInfo l'
	atomically . writeTChan co . expand . Singleton
		. OccCalcTextExtents fn fs txt
		$ mkte ie le
	where
	mkte ie le = TextExtents (r2r ie) (r2r le)
	r2r r = rct
		(pangoRectanglePixelX r) (pangoRectanglePixelY r)
		(pangoRectanglePixelWidth r) (pangoRectanglePixelHeight r)
	rct	(fromIntegral -> l) (fromIntegral -> t)
		(fromIntegral -> w) (fromIntegral -> h) = Rectangle l t w h

type GuiEv = Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent
