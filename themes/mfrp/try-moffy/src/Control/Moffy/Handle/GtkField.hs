{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.GtkField where

import Control.Monad
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Event.Delete as M
import Control.Moffy.Handle
import Control.Concurrent
import Control.Concurrent.STM
import Data.Type.Set
import Data.OneOrMore
import Graphics.Gtk as Gtk

tryGtk :: IO ()
tryGtk = do
	[] <- gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gtkWidgetShowAll w
	gSignalConnect w Destroy gtkMainQuit ()
	gtkMain

tryUseTChan :: IO (TChan (EvOccs (Singleton M.DeleteEvent)))
tryUseTChan = do
	c <- newTChanIO
	void . forkIO $ do
		[] <- gtkInit []
		w <- gtkWindowNew gtkWindowToplevel
		gtkWidgetShowAll w
		gSignalConnect w DeleteEvent (\a b c' -> True <$ (print (a, b, c') >>
			atomically (writeTChan c $ Singleton OccDeleteEvent))) ()
		gSignalConnect w Destroy gtkMainQuit ()
		gtkMain
	pure c

runUseTChan :: IO ()
runUseTChan = do
	c <- tryUseTChan
	atomically (readTChan c)
	gtkMainQuit

handleDelete :: TChan (EvOccs (Singleton M.DeleteEvent)) -> Handle IO (Singleton M.DeleteEvent)
handleDelete c _rqs = atomically $ readTChan c

runHandleDelete :: IO ()
runHandleDelete = do
	c <- tryUseTChan
	interpretReact (handleDelete c) deleteEvent
	gtkMainQuit
