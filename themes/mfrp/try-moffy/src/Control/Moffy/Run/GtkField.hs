{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Run.GtkField (GuiEv, runGtkMain) where

import Prelude hiding (repeat, break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete as M
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse
import Control.Moffy.Event.CalcTextExtents
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Type.Set
import Data.OneOrMore as Oom
import Graphics.Gtk as Gtk
import Graphics.Gtk.Pango

import qualified Data.Text as T

import Data.Bool
import Data.Word

import Data.OneOrMoreApp as Oom

-- GUI EVENT

type GuiEv = CalcTextExtents :- M.DeleteEvent :- KeyEv :+: MouseEv

-- RUN GTK MAIN

runGtkMain :: Monoid a =>
	(GtkWidget -> CairoT -> a -> IO ()) ->
	IO (TChan (EvReqs GuiEv), TChan (EvOccs GuiEv), TChan a)
runGtkMain dr = do
	cr <- newTChanIO
	c <- newTChanIO
	c' <- newTChanIO
	void . forkIO $ do
		tx <- atomically $ newTVar mempty
		ftc <- newTChanIO
		[] <- gtkInit []
		w <- gtkWindowNew gtkWindowToplevel
		gtkWidgetSetEvents w [gdkPointerMotionMask]
		gSignalConnect w DeleteEvent (\_ _ _ -> True <$
			(atomically (writeTChan c . Oom.expandApp $ SingletonApp OccDeleteEvent))) ()
		gSignalConnect w KeyPressEvent (\_ ev _ -> False <$ do
			e <- gdkEventKeyToOccKeyDown ev
			atomically (writeTChan c $ Oom.expandApp e)) ()
		gSignalConnect w KeyReleaseEvent (\_ ev _ -> False <$ do
			e <- gdkEventKeyToOccKeyUp ev
			atomically (writeTChan c $ Oom.expandApp e)) ()
		gSignalConnect w ButtonPressEvent (\_ ev _ -> True <$ do
			e <- gdkEventButtonToOccMouseDown ev
			atomically (writeTChan c $ Oom.expandApp e)) ()
		gSignalConnect w ButtonReleaseEvent (\_ ev _ -> True <$ do
			e <- gdkEventButtonToOccMouseUp ev
			atomically (writeTChan c $ Oom.expandApp e)) ()
		gSignalConnect w MotionNotifyEvent (\_ ev _ -> True <$ do
			e <- gdkEventMotionToOccMouseMove ev
			atomically (writeTChan c $ Oom.expandApp e)) ()
		gSignalConnect w Destroy gtkMainQuit ()
		da <- gtkDrawingAreaNew
		gtkContainerAdd (castWidgetToContainer w) da
		gSignalConnect da DrawEvent (draw dr ftc c tx) ()
		void . flip (gTimeoutAdd 101) () $ const do
			atomically (lastTChan cr) >>= \case
				Nothing -> pure True
				Just rqs -> do
					case project rqs of
						Nothing -> pure True
						Just (CalcTextExtentsReq fn fs t) -> do
							atomically $ writeTChan ftc (fn, fs, t)
							gtkWidgetQueueDraw da
							pure True
		void . flip (gTimeoutAdd 100) () $ const do
			atomically (lastTChan c') >>= \case
				Nothing -> pure True
				Just v -> do
					atomically $ writeTVar tx v
					gtkWidgetQueueDraw da
					pure True
		gtkWidgetShowAll w
		gtkMain
	pure (cr, c, c')

-- GDK EVENT TO OCCURRED EVENT

gdkEventKeyToOccKeyDown :: GdkEventKey -> IO (EvOccs (Singleton KeyDown))
gdkEventKeyToOccKeyDown e =
	SingletonApp . OccKeyDown . Key . fromIntegral <$> keyval e

gdkEventKeyToOccKeyUp :: GdkEventKey -> IO (EvOccs (Singleton KeyUp))
gdkEventKeyToOccKeyUp e =
	SingletonApp . OccKeyUp . Key . fromIntegral <$> keyval e

gdkEventMotionToOccMouseMove ::
	GdkEventMotion -> IO (EvOccs (Singleton MouseMove))
gdkEventMotionToOccMouseMove e = SingletonApp . OccMouseMove
	<$> ((,) <$> gdkEventMotionX e <*> gdkEventMotionY e)

gdkEventButtonToOccMouseDown ::
	GdkEventButton -> IO (EvOccs (MouseDown :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseDown e = do
	p <- (,) <$> gdkEventButtonX e <*> gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure $ OccMouseDown (button b) >-^ SingletonApp (OccMouseMove p)

gdkEventButtonToOccMouseUp ::
	GdkEventButton -> IO (EvOccs (MouseUp :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseUp e = do
	p <- (,) <$> gdkEventButtonX e <*> gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure $ OccMouseUp (button b) >-^ SingletonApp (OccMouseMove p)

button :: Word32 -> MouseBtn
button = \case
	1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
	n -> ButtonUnknown n

-- DRAW

draw :: (GtkWidget -> CairoT -> a -> IO ()) ->
	TChan (FontName, FontSize, T.Text) -> TChan (EvOccs GuiEv) -> TVar a -> GtkWidget -> CairoT -> () -> IO Bool
draw dr ftc co tx w cr () = True <$ do
	m3 <- atomically $ lastTChan ftc
	case m3 of
		Just (fn, fs, txt) -> do
			l <- pangoCairoCreateLayout cr
			pangoLayoutSetText l txt
			d <- pangoFontDescriptionFromString $ T.pack fn
			pangoFontDescriptionSetAbsoluteSize d fs
			pangoLayoutSetFontDescription l d
			te <- pangoLayoutWithPixelExtents l \ie le -> mkTextExtents ie le
			atomically . writeTChan co . Oom.expandApp . SingletonApp
				$ OccCalcTextExtents fn fs txt te
		Nothing -> pure ()
	dr w cr =<< readTVarIO tx

mkTextExtents :: PangoRectangle -> PangoRectangle -> IO TextExtents'
mkTextExtents ie le = TextExtents' <$> r2r ie <*> r2r le
	where
	r2r r = rct
		<$> pangoRectangleX r <*> pangoRectangleY r
		<*> pangoRectangleWidth r <*> pangoRectangleHeight r
	rct	(fromIntegral -> l) (fromIntegral -> t)
		(fromIntegral -> w) (fromIntegral -> h) = Rectangle l t w h

-- HELPER FUNCTION

lastTChan :: TChan a -> STM (Maybe a)
lastTChan c = tryReadTChan c >>= \case
	Nothing -> pure Nothing
	Just x -> bool (lastTChan c) (pure $ Just x) =<< isEmptyTChan c
