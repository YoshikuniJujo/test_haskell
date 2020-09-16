{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Run.GtkField (tryUseTChanGen, GuiEv) where

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
import Data.Int
import Graphics.Gtk as Gtk
import Graphics.Gtk.Cairo
import Graphics.Gtk.Pango

import qualified Data.Text as T

import Data.Bool
import Data.Word

import Data.OneOrMoreApp as Oom

type GuiEv = CalcTextExtents :- M.DeleteEvent :- KeyEv :+: MouseEv

tryUseTChanGen :: Monoid a => (GtkWidget -> CairoT -> a -> IO ()) -> IO (TChan (EvReqs GuiEv), TChan (EvOccs GuiEv), TChan a)
tryUseTChanGen dr = do

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
		gSignalConnect da DrawEvent (tryDraw dr ftc c tx) ()

		void . flip (gTimeoutAdd 101) () $ const do
			atomically (lastTChan cr) >>= \case
				Nothing -> pure True
				Just rqs -> do
					case project rqs of
						Nothing -> pure True
						Just (CalcTextExtentsReq fn fs t) -> do
							putStrLn "CalcTextExtents"
							print (fn, fs, t)
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

lastTChan :: TChan a -> STM (Maybe a)
lastTChan c = do
	tryReadTChan c >>= \case
		Nothing -> pure Nothing
		Just x -> bool (lastTChan c) (pure $ Just x) =<< isEmptyTChan c

tryDraw :: (GtkWidget -> CairoT -> a -> IO ()) ->
	TChan (FontName, FontSize, T.Text) -> TChan (EvOccs GuiEv) -> TVar a -> GtkWidget -> CairoT -> () -> IO Bool
tryDraw dr ftc co tx w cr () = True <$ do
	m3 <- atomically $ lastTChan ftc
	case m3 of
		Just (fn, fs, txt) -> do
			cairoWithTextExtents cr txt $ \e -> do
				print =<< cairoTextExtentsXBearing e
				print =<< cairoTextExtentsYBearing e
				print =<< cairoTextExtentsWidth e
				print =<< cairoTextExtentsHeight e
			l <- pangoCairoCreateLayout cr
			pangoLayoutSetText l txt
			d <- pangoFontDescriptionFromString $ T.pack fn
			pangoFontDescriptionSetAbsoluteSize d fs
			pangoLayoutSetFontDescription l d
			te <- pangoLayoutWithPixelExtents l \ie le -> do
				putStrLn "ink_rect"
				print =<< pangoRectangleX ie
				print =<< pangoRectangleY ie
				print =<< pangoRectangleWidth ie
				print =<< pangoRectangleHeight ie
				putStrLn "logical_rect"
				print =<< pangoRectangleX le
				print =<< pangoRectangleY le
				print =<< pangoRectangleWidth le
				print =<< pangoRectangleHeight le
				mkTextExtents ie le
			atomically . writeTChan co . Oom.expandApp . SingletonApp
				$ OccCalcTextExtents fn fs txt te
		Nothing -> pure ()
	dr w cr =<< readTVarIO tx

rectangle :: Int32 -> Int32 -> Int32 -> Int32 -> Rectangle
rectangle (fromIntegral -> l) (fromIntegral -> t) (fromIntegral -> w) (fromIntegral -> h) =
	Rectangle l t w h

mkTextExtents :: PangoRectangle -> PangoRectangle -> IO TextExtents'
mkTextExtents ie le = TextExtents'
	<$> (rectangle <$> pangoRectangleX ie <*> pangoRectangleY ie <*> pangoRectangleWidth ie <*> pangoRectangleHeight ie)
	<*> (rectangle <$> pangoRectangleX le <*> pangoRectangleY le <*> pangoRectangleWidth le <*> pangoRectangleHeight le)

gdkEventKeyToOccKeyDown :: GdkEventKey -> IO (EvOccs (Singleton KeyDown))
gdkEventKeyToOccKeyDown e = do
	kv <- keyval e
	pure . SingletonApp . OccKeyDown . Key $ fromIntegral kv

gdkEventKeyToOccKeyUp :: GdkEventKey -> IO (EvOccs (Singleton KeyUp))
gdkEventKeyToOccKeyUp e = do
	kv <- keyval e
	pure . SingletonApp . OccKeyUp . Key $ fromIntegral kv

gdkEventMotionToOccMouseMove :: GdkEventMotion -> IO (EvOccs (Singleton MouseMove))
gdkEventMotionToOccMouseMove e = do
	x <- gdkEventMotionX e
	y <- gdkEventMotionY e
	pure $ SingletonApp $ OccMouseMove (x, y)

gdkEventButtonToOccMouseDown :: GdkEventButton -> IO (EvOccs (MouseDown :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseDown e = do
	(x, y, b) <- getButtonInfo e
	pure $ OccMouseDown (btn b) >-^ SingletonApp (OccMouseMove (x, y))
	where
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		n -> ButtonUnknown n

getButtonInfo :: GdkEventButton -> IO (Double, Double, Word32)
getButtonInfo e = do
	x <- gdkEventButtonX e
	y <- gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure (x, y, b)

gdkEventButtonToOccMouseUp :: GdkEventButton -> IO (EvOccs (MouseUp :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseUp e = do
	(x, y, b) <- getButtonInfo e
	pure $ OccMouseUp (btn b) >-^ SingletonApp (OccMouseMove (x, y))
	where
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		n -> ButtonUnknown n
