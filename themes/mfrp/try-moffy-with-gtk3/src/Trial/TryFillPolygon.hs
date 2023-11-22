{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryFillPolygon where

import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Viewable.Basic
import Control.Moffy.Viewable.Shape
import Control.Moffy.Run.TChan
import Control.Moffy.Run.GtkField
import Data.Type.Set
import Data.OneOfThem as Oot
import Graphics.Gtk

import Data.Map
import Control.Moffy.Event.Window

tryFillPolygon :: Sig s GuiEv [OneOfThem (Box :- FillPolygon :- 'Nil)] ()
tryFillPolygon = do
	emit [	Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Blue,
		Oot.expand . Singleton $ FillPolygon (Color 0x7f 0xef 0x3f) [(150, 50), (200, 80), (190, 100), (160, 110), (120, 90)]
		]
	waitFor never

runFillPolygon :: (Monoid a, Adjustable es GuiEv) => GtkDrawer' a -> Sig s es (Map WindowId a) r -> IO r
runFillPolygon dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	interpret (retry $ handle Nothing cr c) c' s <* gtkMainQuit
