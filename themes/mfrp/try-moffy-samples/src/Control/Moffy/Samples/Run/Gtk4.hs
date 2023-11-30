{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Run.Gtk4 where

import Foreign.C.Types
import Control.Monad
import Control.Monad.ST
import Control.Concurrent
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.View
import Control.Moffy.Samples.Followbox.Event.CalcTextExtents
import Data.Type.Set
import Data.OneOrMore qualified as OOM
import Data.OneOrMoreApp
import Data.Maybe
import Data.Int
import Data.Text qualified as T
import Data.Color
import Stopgap.Data.Ptr
import System.Environment
import System.Exit

import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Rendering.Cairo

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.ApplicationWindow qualified as
	Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.Graphics.UI.Gtk.EventControllerMotion qualified as
	Gtk.EventControllerMotion
import Stopgap.Graphics.UI.Gtk.GestureClick qualified as Gtk.GestureClick
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.System.GLib.Idle qualified as G.Idle

beforeClose :: TChan (EvOccs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	Gtk.ApplicationWindow.A -> Null -> IO Bool
beforeClose ceo win Null = do
	putStrLn "BEFORE CLOSE"
	atomically $ writeTChan ceo (expand $ Singleton OccDeleteEvent)
	pure True

appActivate ::
	TChan (EvReqs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	TChan (EvOccs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	TChan View ->
	Gtk.Application.A s -> Null -> IO ()
appActivate cer ceo cv app Null = do
	crd <- atomically $ newTVar []
	cte <- atomically newTChan
	win <- Gtk.ApplicationWindow.new app
	da <- Gtk.DrawingArea.new
	Gtk.DrawingArea.setDrawFunc da (drawFunction crd ceo cte) Null

	gcp <- mouseButtonHandler ceo Mouse.ButtonPrimary
	gcm <- mouseButtonHandler ceo Mouse.ButtonMiddle
	gcs <- mouseButtonHandler ceo Mouse.ButtonSecondary

	ecm <- Gtk.EventControllerMotion.new
	G.Signal.connectXY ecm (G.Signal.Signal "motion") (moveHandler ceo) Null

	Gtk.Window.setChild win da
	Gtk.Widget.addController da gcp
	Gtk.Widget.addController da gcm
	Gtk.Widget.addController da gcs
	Gtk.Widget.addController da ecm

	forkIO . forever $ atomically (readTChan cer) >>= \r -> do
		case OOM.project r of
			Nothing -> pure ()
			Just (CalcTextExtentsReq fn fs t) -> do
				atomically $ writeTChan cte (fn, fs, t)
				void $ G.Idle.add
					(\_ ->	Gtk.Widget.queueDraw da >>
						pure False)
					Null

	forkIO . forever $ atomically (readTChan cv) >>= \case
		Stopped -> void $ G.Idle.add
			(\_ -> Gtk.Window.destroy win >> pure False) Null
		View v -> do
			atomically $ writeTVar crd v
			void $ G.Idle.add
				(\_ -> Gtk.Widget.queueDraw da >> pure False)
				Null
		v -> print v

	G.Signal.connectClose win (G.Signal.Signal "close-request") (beforeClose ceo) Null

	Gtk.Window.present win

appId :: Gtk.Application.Id
appId = Gtk.Application.Id "com.github.YoshikuniJujo.moffy-samples-run"

runSingleWin ::
	TChan (EvReqs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	TChan (EvOccs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	TChan View -> IO ()
runSingleWin cer ceo cv = Gtk.Application.with
		appId G.Application.DefaultFlags \app -> do
	G.Signal.connect app (G.Signal.Signal "activate") (appActivate cer ceo cv) Null
	exitWith =<< join (G.Application.run app <$> getProgName <*> getArgs)

pressHandler ::
	TChan (EvOccs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	Mouse.Button ->
	Gtk.GestureClick.G -> Int32 -> Double -> Double -> Null -> IO ()
pressHandler ceo b _gc n x y Null = atomically . writeTChan ceo
	$ expand (Mouse.OccMove (x, y) >- Singleton (Mouse.OccDown b) ::
		EvOccs (Mouse.Move :- Singleton Mouse.Down))

releaseHandler ::
	TChan (EvOccs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	Mouse.Button ->
	Gtk.GestureClick.G -> Int32 -> Double -> Double -> Null -> IO ()
releaseHandler ceo b _gc n x y Null = atomically . writeTChan ceo
	$ expand (Mouse.OccMove (x, y) >- Singleton (Mouse.OccUp b) ::
		EvOccs (Mouse.Move :- Singleton Mouse.Up))

mouseButtonHandler ::
	TChan (EvOccs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	Mouse.Button ->
	IO Gtk.GestureClick.G
mouseButtonHandler ceo b = do
	gcp <- Gtk.GestureClick.new
	Gtk.GestureClick.setButton gcp (mouseButtonToGesture b)
	G.Signal.connectNXY gcp
		(G.Signal.Signal "pressed") (pressHandler ceo b) Null
	G.Signal.connectNXY gcp
		(G.Signal.Signal "released") (releaseHandler ceo b) Null
	pure gcp

moveHandler ::
	TChan (EvOccs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	Gtk.EventControllerMotion.E -> Double -> Double -> Null -> IO ()
moveHandler ceo _ x y Null = atomically . writeTChan ceo
	. expand $ Singleton (Mouse.OccMove (x, y))

type GuiEv = Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent

viewHandler :: TChan view -> Gtk.ApplicationWindow.A -> IO Bool
viewHandler cv win = do
	atomically $ readTChan cv
	putStrLn "VIEW HANDLER"
	pure True

mouseButtonToGesture :: Mouse.Button -> Gtk.GestureClick.Button
mouseButtonToGesture = \case
	Mouse.ButtonPrimary -> Gtk.GestureClick.ButtonPrimary
	Mouse.ButtonMiddle -> Gtk.GestureClick.ButtonMiddle
	Mouse.ButtonSecondary -> Gtk.GestureClick.ButtonSecondary

drawFunction :: TVar [View1] ->
	TChan (EvOccs (CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent)) ->
	TChan (FontName, FontSize, T.Text) ->
	Gtk.DrawingArea.DrawFunction r Null
drawFunction crd ceo cte area cr width height Null = do
	atomically (tryReadTChan cte) >>= \case
		Nothing -> pure ()
		Just (fn, fs, txt) -> occCalcTextExtents ceo cr fn fs txt
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoPaint cr
	cairoSetLineWidth cr 2
	(drawView1 cr `mapM_`) =<< atomically (readTVar crd)

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
