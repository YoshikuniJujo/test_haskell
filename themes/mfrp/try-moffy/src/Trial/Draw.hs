{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw where

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Viewable.Basic
import Control.Moffy.Viewable.Shape
import Data.Type.Set

import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Moffy.Run.GtkField

import Graphics.Gtk hiding (DeleteEvent)

import Data.OneOfThem

sampleLine :: Sig s (Singleton DeleteEvent) [OneOfThem (Singleton Line)] ()
sampleLine = do
	emit [Singleton $ Line' (Color 0 0 0) 2 (50, 50) (200, 200)]
	waitFor deleteEvent

runDraw :: Monoid a => GtkDrawer a -> Sig s (Singleton DeleteEvent) a r -> IO r
runDraw dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	interpret (retry $ handle Nothing cr c) c' s <* gtkMainQuit
