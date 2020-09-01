{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.GtkField where

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

tryUseTChan :: IO (TChan (EvOccs (M.DeleteEvent :- MouseMove :- 'Nil)))
tryUseTChan = do
	c <- newTChanIO
	void . forkIO $ do
		[] <- gtkInit []
		w <- gtkWindowNew gtkWindowToplevel
		gtkWidgetShowAll w
		gSignalConnect w DeleteEvent (\a b c' -> True <$ (print (a, b, c') >>
			atomically (writeTChan c . Oom.expand $ Singleton OccDeleteEvent))) ()
		gSignalConnect w MotionNotifyEvent (\a b c' -> False <$ (print (a, b, c') >>
			atomically (writeTChan c . Oom.expand $ Singleton (OccMouseMove (0, 0))))) ()
		gSignalConnect w Destroy gtkMainQuit ()
		gtkMain
	pure c

runUseTChan :: IO ()
runUseTChan = do
	c <- tryUseTChan
	atomically (readTChan c)
	gtkMainQuit

handleDelete :: TChan (EvOccs (M.DeleteEvent :- MouseMove :- 'Nil)) -> Handle IO (M.DeleteEvent :- MouseMove :- 'Nil)
handleDelete c _rqs = atomically $ readTChan c

runHandleDelete :: IO ()
runHandleDelete = do
	c <- tryUseTChan
	interpretReact (handleDelete c) $ deleteEvent `first` mouseMove
	gtkMainQuit
