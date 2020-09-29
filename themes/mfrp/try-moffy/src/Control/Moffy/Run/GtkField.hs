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

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete as M (DeleteEvent, pattern OccDeleteEvent)
import Control.Moffy.Event.Window
import Control.Moffy.Event.Key (
	KeyEv, pattern OccKeyDown, pattern OccKeyUp, Key(..) )
import Control.Moffy.Event.Mouse (
	MouseEv,
	MouseDown, pattern OccMouseDown,  MouseUp, pattern OccMouseUp,
	MouseMove, pattern OccMouseMove, MouseBtn(..),
	MouseScroll, pattern OccMouseScroll )
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

import Data.Map as Map

-- GUI EVENT

type GuiEv = WindowEv :+: CalcTextExtents :- M.DeleteEvent :- KeyEv :+: MouseEv

-- RUN GTK MAIN

type GtkDrawer a = GtkWidget -> CairoT -> a -> IO ()

runGtkMain :: Monoid a =>
	GtkDrawer a -> [String] -> IO ([String], (TChan (EvReqs GuiEv), TChan (EvOccs GuiEv), TChan (Map WindowId a)))
runGtkMain dr as = (,,) <$> newTChanIO <*> newTChanIO <*> newTChanIO >>=
	\cs@(crq, cocc, cvw) -> (, cs) <$> do
		cas <- newTChanIO
		void $ forkIO do
			vwid <- atomically . newTVar $ WindowId 0
			vw <- atomically $ newTVar empty
			vda <- atomically $ newTVar empty
			vvw <- atomically $ newTVar empty
			cft <- newTChanIO
			atomically . writeTChan cas =<< gtkInit as
			void $ gTimeoutAdd 100
				(const $ True <$ recieveEvReqs dr vwid vw crq cft cocc vvw vda) ()
			void $ gTimeoutAdd 100 (const $ recieveViewable cvw vvw vda) ()
			gtkMain
		atomically $ readTChan cas

recieveEvReqs :: Monoid a => GtkDrawer a -> TVar WindowId -> TVar (Map WindowId GtkWidget) -> TChan (EvReqs GuiEv) -> TChan (WindowId, FontName, FontSize, T.Text) ->
	TChan (EvOccs GuiEv) -> TVar (Map WindowId a) -> TVar (Map WindowId GtkWidget) -> IO ()
recieveEvReqs dr vwid vw crq cft cocc vvw vda = atomically (lastTChan crq) >>= \case
	Nothing -> pure ()
	Just rqs -> do
		case project rqs of
			Nothing -> pure ()
			Just (CalcTextExtentsReq wid fn fs t) -> do
				atomically (readTVar vda) >>= \das -> case Map.lookup wid das of
					Nothing -> pure ()
					Just da -> do
						atomically $ writeTChan cft (wid, fn, fs, t)
						gtkWidgetQueueDraw da
		case project rqs of
			Nothing -> pure ()
			Just WindowNewReq -> do
				putStrLn "recieve WindowNewReq begin"
				i <- atomically $ do
					wid@(WindowId n) <- readTVar vwid
					writeTVar vwid $ WindowId (n + 1)
					pure wid
				w <- createWindow i cocc
				atomically $ do
					ws <- readTVar vw
					writeTVar vw $ insert i w ws
				da <- createDrawingArea i dr cft cocc vvw w
				gtkWidgetShowAll w
				atomically $ do
					das <- readTVar vda
					writeTVar vda (insert i da das)
					writeTChan cocc . expand . Singleton $ OccWindowNew i
				putStrLn "recieve WindowNewReq end"
		case project rqs of
			Nothing -> pure ()
			Just (WindowDestroyReq i) -> do
				ws <- atomically $ readTVar vw
				case Map.lookup i ws of
					Nothing -> putStrLn "no window to destroy" >> pure ()
--					Just w -> putStrLn "not destroy"
					Just w -> putStrLn "destroy window" >> gtkWidgetDestroy w >> putStrLn "window has been destroyed"
				atomically $ writeTChan cocc . expand . Singleton $ OccWindowDestroy i
				putStrLn $ "send OccWindowDestroy " ++ show i

createWindow :: WindowId -> TChan (EvOccs GuiEv) -> IO GtkWidget
createWindow wid c = do
	w <- gtkWindowNew gtkWindowToplevel
	gtkWidgetSetEvents w [gdkPointerMotionMask, gdkScrollMask]
	w <$ mapM_ ($ ()) [
--		gSignalConnect w Destroy gtkMainQuit,
		gSignalConnect w DeleteEvent \_ _ _ -> deleteEvent wid c,
		gSignalConnect w KeyPressEvent \_ ev _ -> keyDown wid c ev,
		gSignalConnect w KeyReleaseEvent \_ ev _ -> keyUp wid c ev,
		gSignalConnect w ButtonPressEvent \_ ev _ -> buttonDown wid c ev,
		gSignalConnect w ButtonReleaseEvent \_ ev _ -> buttonUp wid c ev,
		gSignalConnect w ScrollEvent \_ ev _ -> True <$ do
			x <- gdkEventScrollX ev
			y <- gdkEventScrollY ev
			dx <- gdkEventScrollDeltaX ev
			dy <- gdkEventScrollDeltaY ev
			atomically . writeTChan c $ expand (
				OccMouseMove wid (x, y) >- Singleton (OccMouseScroll wid dx dy) ::
					EvOccs (MouseMove :- MouseScroll :- 'Nil) )
			,
		gSignalConnect w MotionNotifyEvent \_ ev _ -> mouseMove wid c ev ]

createDrawingArea :: Monoid a => WindowId -> GtkDrawer a -> TChan (WindowId, FontName, FontSize, T.Text) ->
	TChan (EvOccs GuiEv) -> TVar (Map WindowId a) -> GtkWidget -> IO GtkWidget
createDrawingArea wid dr ftc c tx w = do
	da <- gtkDrawingAreaNew
	gtkContainerAdd (castWidgetToContainer w) da
	da <$ gSignalConnect da DrawEvent (draw wid dr ftc c tx) ()

recieveViewable :: TChan (Map WindowId a) -> TVar (Map WindowId a) -> TVar (Map WindowId GtkWidget) -> IO Bool
recieveViewable c' tx vda = atomically (readTVar vda) >>= \das -> True <$ do
		atomically (lastTChan c') >>= maybe (pure ()) \v ->
			atomically (writeTVar tx v) >> (gtkWidgetQueueDraw . snd) `mapM_` Map.toList das

-- HANDLER OF GDK EVENT

deleteEvent :: WindowId -> TChan (EvOccs GuiEv) -> IO Bool
deleteEvent wid c = True <$ do
	putStrLn $ "deleteEvent occur: " ++ show wid
	atomically . writeTChan c . expand . Singleton $ OccDeleteEvent wid

keyDown, keyUp :: WindowId -> TChan (EvOccs GuiEv) -> GdkEventKey -> IO Bool
keyDown wid c ev = (False <$) $ atomically . writeTChan c . expand
	=<< Singleton . OccKeyDown wid . Key . fromIntegral <$> keyval ev

keyUp wid c ev = (False <$) $ atomically . writeTChan c . expand
	=<< Singleton . OccKeyUp wid . Key . fromIntegral <$> keyval ev

buttonDown, buttonUp :: WindowId -> TChan (EvOccs GuiEv) -> GdkEventButton -> IO Bool
buttonDown wid c ev = (True <$) $ atomically . writeTChan c . expand =<< occ
	where
	occ :: IO (EvOccs (MouseDown :- MouseMove :- 'Nil))
	occ = do
		(b, p) <- (,) <$> gdkEventButtonButton ev
			<*> ((,) <$> gdkEventButtonX ev <*> gdkEventButtonY ev)
		pure $ OccMouseDown wid (button b) >- Singleton (OccMouseMove wid p)

buttonUp wid c ev = (True <$) $ atomically . writeTChan c . expand =<< occ
	where
	occ :: IO (EvOccs (MouseUp :- MouseMove :- 'Nil))
	occ = do
		(b, p) <- (,) <$> gdkEventButtonButton ev
			<*> ((,) <$> gdkEventButtonX ev <*> gdkEventButtonY ev)
		pure $ OccMouseUp wid (button b) >- Singleton (OccMouseMove wid p)

mouseMove :: WindowId -> TChan (EvOccs GuiEv) -> GdkEventMotion -> IO Bool
mouseMove wid c ev = (True <$)
	$ atomically . writeTChan c . expand . Singleton . OccMouseMove wid
		=<< ((,) <$> gdkEventMotionX ev <*> gdkEventMotionY ev)

button :: Word32 -> MouseBtn
button = \case
	1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
	n -> ButtonUnknown n

-- DRAW

draw :: Monoid a => WindowId -> GtkDrawer a -> TChan (WindowId, FontName, FontSize, T.Text) ->
	TChan (EvOccs GuiEv) -> TVar (Map WindowId a) -> GtkWidget -> CairoT -> () -> IO Bool
draw wid dr ftc co tx wdgt cr () = True <$ do
	atomically (lastTChan ftc) >>= maybe (pure ()) \(wid', fn, fs, txt) -> case wid' == wid of
		True -> do
			(l, d) <- (,) <$> pangoCairoCreateLayout cr
				<*> pangoFontDescriptionFromString (T.pack fn)
			d `pangoFontDescriptionSetAbsoluteSize` fs
			l `pangoLayoutSetFontDescription` d
			l `pangoLayoutSetText` txt
			atomically . writeTChan co . expand . Singleton
					. OccCalcTextExtents wid fn fs txt
				=<< pangoLayoutWithPixelExtents l \ie le -> mkte ie le
		False -> atomically (writeTChan ftc (wid', fn, fs, txt))
	readTVarIO tx >>= \mx -> case Map.lookup wid mx of
		Nothing -> dr wdgt cr mempty
		Just x -> dr wdgt cr x
	where
	mkte ie le = TextExtents <$> r2r ie <*> r2r le
	r2r r = rct
		<$> pangoRectangleX r <*> pangoRectangleY r
		<*> pangoRectangleWidth r <*> pangoRectangleHeight r
	rct	(fromIntegral -> l) (fromIntegral -> t)
		(fromIntegral -> w) (fromIntegral -> h) = Rectangle l t w h

-- HELPER FUNCTION

lastTChan :: TChan a -> STM (Maybe a)
lastTChan c = tryReadTChan c >>= maybe (pure Nothing)
	((isEmptyTChan c >>=) . bool (lastTChan c) . (pure . Just))
