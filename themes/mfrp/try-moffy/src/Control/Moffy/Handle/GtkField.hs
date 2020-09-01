{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.GtkField where

import Prelude hiding (repeat, break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Event.Delete as M
import Control.Moffy.Event.Mouse
import Control.Moffy.Handle
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)
import Data.Type.Set
import Data.OneOrMore as Oom
import Data.Time
import System.Timeout
import Graphics.Gtk as Gtk

import Control.Moffy.Event.Time
import Control.Moffy.Handle.Time
import Data.Time.Clock.TAI

tryGtk :: IO ()
tryGtk = do
	[] <- gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gtkWidgetShowAll w
	gSignalConnect w Destroy gtkMainQuit ()
	gtkMain

tryUseTChan :: IO (TChan (EvOccs (M.DeleteEvent :- MouseEv)))
tryUseTChan = do
	c <- newTChanIO
	void . forkIO $ do
		[] <- gtkInit []
		w <- gtkWindowNew gtkWindowToplevel
		gtkWidgetSetEvents w [gdkPointerMotionMask]
		gtkWidgetShowAll w
		gSignalConnect w DeleteEvent (\a b c' -> True <$ (print (a, b, c') >>
			atomically (writeTChan c . Oom.expand $ Singleton OccDeleteEvent))) ()
		gSignalConnect w ButtonPressEvent (\a b c' -> False <$ (print (a, b, c') >> do
			e <- gdkEventButtonToOccMouseDown b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w ButtonReleaseEvent (\a b c' -> False <$ (print (a, b, c') >> do
			e <- gdkEventButtonToOccMouseUp b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w MotionNotifyEvent (\a b c' -> False <$ (print (a, b, c') >> do
			e <- gdkEventMotionToOccMouseMove b
			atomically (writeTChan c $ Oom.expand e))) ()
		gSignalConnect w Destroy gtkMainQuit ()
		gtkMain
	pure c

gdkEventMotionToOccMouseMove :: GdkEventMotion -> IO (EvOccs (Singleton MouseMove))
gdkEventMotionToOccMouseMove e = do
	x <- round <$> gdkEventMotionX e
	y <- round <$> gdkEventMotionY e
	pure $ Singleton $ OccMouseMove (x, y)

gdkEventButtonToOccMouseDown :: GdkEventButton -> IO (EvOccs (MouseDown :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseDown e = do
	x <- round <$> gdkEventButtonX e
	y <- round <$> gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure $ OccMouseDown (btn b) >- Singleton (OccMouseMove (x, y))
	where
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		n -> ButtonUnknown n

gdkEventButtonToOccMouseUp :: GdkEventButton -> IO (EvOccs (MouseUp :- MouseMove :- 'Nil))
gdkEventButtonToOccMouseUp e = do
	x <- round <$> gdkEventButtonX e
	y <- round <$> gdkEventButtonY e
	b <- gdkEventButtonButton e
	pure $ OccMouseUp (btn b) >- Singleton (OccMouseMove (x, y))
	where
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		n -> ButtonUnknown n

runUseTChan :: IO ()
runUseTChan = do
	c <- tryUseTChan
	atomically (readTChan c)
	gtkMainQuit

handleDelete :: DiffTime -> TChan (EvOccs (M.DeleteEvent :- MouseEv)) -> Handle' IO (M.DeleteEvent :- MouseEv)
handleDelete t c _rqs = timeout (round $ t * 1000000) . atomically $ readTChan c

runHandleDelete :: IO ()
runHandleDelete = do
	c <- tryUseTChan
	interpret (retry $ handleDelete 0.1 c) print $ repeat (mouseMove `first` mouseDown `first` mouseUp)  `break` deleteEvent
	gtkMainQuit

handleBoxesFoo :: DiffTime -> TChan (EvOccs (M.DeleteEvent :- MouseEv)) -> HandleSt (Mode, AbsoluteTime) IO (TimeEv :+: M.DeleteEvent :- MouseEv)
handleBoxesFoo = ((retrySt .) .) . curry . popInput . handleTimeEvPlus
	. pushInput . uncurry $ (liftHandle' .) . handleDelete
