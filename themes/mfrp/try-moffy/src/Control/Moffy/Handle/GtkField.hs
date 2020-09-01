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
import Control.Concurrent.STM
import Data.Type.Set
import Data.OneOrMore as Oom
import Graphics.Gtk as Gtk

tryGtk :: IO ()
tryGtk = do
	[] <- gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gtkWidgetShowAll w
	gSignalConnect w Destroy gtkMainQuit ()
	gtkMain

tryUseTChan :: IO (TChan (EvOccs (M.DeleteEvent :- MouseDown :- MouseMove :- 'Nil)))
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

runUseTChan :: IO ()
runUseTChan = do
	c <- tryUseTChan
	atomically (readTChan c)
	gtkMainQuit

handleDelete :: TChan (EvOccs (M.DeleteEvent :- MouseDown :- MouseMove :- 'Nil)) -> Handle IO (M.DeleteEvent :- MouseDown :- MouseMove :- 'Nil)
handleDelete c _rqs = atomically $ readTChan c

runHandleDelete :: IO ()
runHandleDelete = do
	c <- tryUseTChan
	interpret (handleDelete c) print $ repeat (mouseMove `first` mouseDown)  `break` deleteEvent
	gtkMainQuit
