{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryPngCursor where

import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Cursor
import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.GtkField
import Control.Moffy.Run.TChan
import Data.Type.Set
import qualified Data.Map as Map
import System.Environment
import Graphics.Gtk (gtkMainQuit)

import Trial.Draw.Viewable

import qualified Data.ByteString as BS

tryPngCursor :: BS.ByteString -> Sig s (WindowNew :- DeleteEvent :- SetCursorFromPng :- 'Nil) (Map.Map WindowId [Message]) ()
tryPngCursor bs = do
	i <- waitFor $ adjust windowNew
	waitFor . adjust . setCursorFromPng i $ PngCursor 0 0 bs
	waitFor . adjust $ deleteEvent i

runTryPngCursor :: IO ()
runTryPngCursor = do
	fp : _ <- getArgs
	png <- BS.readFile fp
	([], (cr, c, c')) <- runGtkMain (\_ _ -> mapM_ putMessage) []
	interpret (retry $ handle Nothing cr c) c' (tryPngCursor png)
	gtkMainQuit
