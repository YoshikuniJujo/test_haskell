{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Run.GtkField (
	-- * Gui Event
	GuiEv,
	-- * Run Gtk Main
	GtkDrawer, runGtkMain) where

import Prelude hiding (repeat, break)

import Control.Arrow
import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete as M (DeleteEvent, pattern OccDeleteEvent)
import Control.Moffy.Event.Key (
	KeyEv, pattern OccKeyDown, pattern OccKeyUp, Key(..) )
import Control.Moffy.Event.Mouse (
	MouseEv,
	MouseDown, pattern OccMouseDown,  MouseUp, pattern OccMouseUp,
	MouseMove, pattern OccMouseMove, MouseBtn(..) )
import Control.Moffy.Event.CalcTextExtents
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Type.Set
import Data.OneOrMore (project)
import Graphics.Gtk as Gtk
import Graphics.Gtk.Pango

import qualified Data.Text as T

import Data.Bool
import Data.Word

import Data.OneOrMoreApp (pattern Singleton, expand, (>-))

-- GUI EVENT

type GuiEv = CalcTextExtents :- M.DeleteEvent :- KeyEv :+: MouseEv

-- RUN GTK MAIN

type GtkDrawer a = GtkWidget -> CairoT -> a -> IO ()

runGtkMain :: Monoid a =>
	GtkDrawer a -> [String] -> IO ([String], (TChan (EvReqs GuiEv), TChan (EvOccs GuiEv), TChan a))
runGtkMain dr as = (,,) <$> newTChanIO <*> newTChanIO <*> newTChanIO >>=
	\cs@(crq, cocc, cvw) -> (, cs) <$> do
		cas <- newTChanIO
		void $ forkIO do
			vvw <- atomically $ newTVar mempty
			cft <- newTChanIO
			atomically . writeTChan cas =<< gtkInit as
			w <- createWindow cocc
			da <- createDrawingArea dr cft cocc vvw w
			void $ gTimeoutAdd 100
				(const $ recieveCalcTextExtentsRequest crq cft da) ()
			void $ gTimeoutAdd 100 (const $ recieveViewable cvw vvw da) ()
			gtkWidgetShowAll w
			gtkMain
		atomically $ readTChan cas

createWindow :: TChan (EvOccs GuiEv) -> IO GtkWidget
createWindow c = do
	w <- gtkWindowNew gtkWindowToplevel
	gtkWidgetSetEvents w [gdkPointerMotionMask, gdkScrollMask]
	w <$ mapM_ ($ ()) [
		gSignalConnect w Destroy gtkMainQuit,
		gSignalConnect w DeleteEvent \_ _ _ -> deleteEvent c,
		gSignalConnect w KeyPressEvent \_ ev _ -> keyDown c ev,
		gSignalConnect w KeyReleaseEvent \_ ev _ -> keyUp c ev,
		gSignalConnect w ButtonPressEvent \_ ev _ -> buttonDown c ev,
		gSignalConnect w ButtonReleaseEvent \_ ev _ -> buttonUp c ev,
		gSignalConnect w ScrollEvent \_ ev _ -> True <$ do
			print ev
			print =<< gdkEventScrollDeltaX ev
			print =<< gdkEventScrollDeltaY ev
			,
		gSignalConnect w MotionNotifyEvent \_ ev _ -> mouseMove c ev ]

createDrawingArea :: GtkDrawer a -> TChan (FontName, FontSize, T.Text) ->
	TChan (EvOccs GuiEv) -> TVar a -> GtkWidget -> IO GtkWidget
createDrawingArea dr ftc c tx w = do
	da <- gtkDrawingAreaNew
	gtkContainerAdd (castWidgetToContainer w) da
	da <$ gSignalConnect da DrawEvent (draw dr ftc c tx) ()

recieveCalcTextExtentsRequest ::
	TChan (EvReqs GuiEv) -> TChan (FontName, FontSize, T.Text) -> GtkWidget -> IO Bool
recieveCalcTextExtentsRequest cr ftc da = (True <$) $ atomically (lastTChan cr) >>=
	maybe (pure ()) (project >>> maybe (pure ()) \(CalcTextExtentsReq fn fs t) ->
		atomically (writeTChan ftc (fn, fs, t)) >> gtkWidgetQueueDraw da)

recieveViewable :: TChan a -> TVar a -> GtkWidget -> IO Bool
recieveViewable c' tx da = (True <$) $ atomically (lastTChan c') >>= maybe (pure ()) \v ->
	atomically (writeTVar tx v) >> gtkWidgetQueueDraw da

-- HANDLER OF GDK EVENT

deleteEvent :: TChan (EvOccs GuiEv) -> IO Bool
deleteEvent c = (True <$)
	. atomically . writeTChan c . expand $ Singleton OccDeleteEvent

keyDown, keyUp :: TChan (EvOccs GuiEv) -> GdkEventKey -> IO Bool
keyDown c ev = (False <$) $ atomically . writeTChan c . expand
	=<< Singleton . OccKeyDown . Key . fromIntegral <$> keyval ev

keyUp c ev = (False <$) $ atomically . writeTChan c . expand
	=<< Singleton . OccKeyUp . Key . fromIntegral <$> keyval ev

buttonDown, buttonUp :: TChan (EvOccs GuiEv) -> GdkEventButton -> IO Bool
buttonDown c ev = (True <$) $ atomically . writeTChan c . expand =<< occ
	where
	occ :: IO (EvOccs (MouseDown :- MouseMove :- 'Nil))
	occ = do
		(b, p) <- (,) <$> gdkEventButtonButton ev
			<*> ((,) <$> gdkEventButtonX ev <*> gdkEventButtonY ev)
		pure $ OccMouseDown (button b) >- Singleton (OccMouseMove p)

buttonUp c ev = (True <$) $ atomically . writeTChan c . expand =<< occ
	where
	occ :: IO (EvOccs (MouseUp :- MouseMove :- 'Nil))
	occ = do
		(b, p) <- (,) <$> gdkEventButtonButton ev
			<*> ((,) <$> gdkEventButtonX ev <*> gdkEventButtonY ev)
		pure $ OccMouseUp (button b) >- Singleton (OccMouseMove p)

mouseMove :: TChan (EvOccs GuiEv) -> GdkEventMotion -> IO Bool
mouseMove c ev = (True <$)
	$ atomically . writeTChan c . expand . Singleton . OccMouseMove
		=<< ((,) <$> gdkEventMotionX ev <*> gdkEventMotionY ev)

button :: Word32 -> MouseBtn
button = \case
	1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
	n -> ButtonUnknown n

-- DRAW

draw :: GtkDrawer a -> TChan (FontName, FontSize, T.Text) ->
	TChan (EvOccs GuiEv) -> TVar a -> GtkWidget -> CairoT -> () -> IO Bool
draw dr ftc co tx wdgt cr () = True <$ do
	atomically (lastTChan ftc) >>= maybe (pure ()) \(fn, fs, txt) -> do
		(l, d) <- (,) <$> pangoCairoCreateLayout cr
			<*> pangoFontDescriptionFromString (T.pack fn)
		d `pangoFontDescriptionSetAbsoluteSize` fs
		l `pangoLayoutSetFontDescription` d
		l `pangoLayoutSetText` txt
		atomically . writeTChan co . expand . Singleton
				. OccCalcTextExtents fn fs txt
			=<< pangoLayoutWithPixelExtents l \ie le -> mkte ie le
	dr wdgt cr =<< readTVarIO tx
	where
	mkte ie le = TextExtents' <$> r2r ie <*> r2r le
	r2r r = rct
		<$> pangoRectangleX r <*> pangoRectangleY r
		<*> pangoRectangleWidth r <*> pangoRectangleHeight r
	rct	(fromIntegral -> l) (fromIntegral -> t)
		(fromIntegral -> w) (fromIntegral -> h) = Rectangle l t w h

-- HELPER FUNCTION

lastTChan :: TChan a -> STM (Maybe a)
lastTChan c = tryReadTChan c >>= maybe (pure Nothing)
	((isEmptyTChan c >>=) . bool (lastTChan c) . (pure . Just))
