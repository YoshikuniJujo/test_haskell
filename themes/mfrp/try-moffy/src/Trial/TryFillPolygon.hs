{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryFillPolygon where

import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Viewable.Shape
import Control.Moffy.View.GtkField
import Control.Moffy.Run.TChan
import Control.Moffy.Run.GtkField
import Data.Type.Set
import Data.OneOfThem
import Graphics.Gtk

tryFillPolygon :: Sig s GuiEv [OneOfThem (Singleton Box)] ()
tryFillPolygon = do
	emit [Singleton $ Box (Rect (50, 50) (100, 100)) Blue]
	waitFor never

runFillPolygon :: (Monoid a, Adjustable es GuiEv) => GtkDrawer a -> Sig s es a r -> IO r
runFillPolygon dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	interpret (retry $ handle Nothing cr c) c' s <* gtkMainQuit
