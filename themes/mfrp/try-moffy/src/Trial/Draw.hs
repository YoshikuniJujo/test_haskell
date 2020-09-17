{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw where

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Mouse
import Control.Moffy.Viewable.Basic
import Control.Moffy.Viewable.Shape
import Data.Type.Set

import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Moffy.Run.GtkField

import Graphics.Gtk hiding (DeleteEvent)

import Data.OneOfThem

maybeEither :: b -> Either a (Maybe b, ()) -> b
maybeEither d (Left _) = d
maybeEither d (Right (Nothing, ())) = d
maybeEither _ (Right (Just x, ())) = x

sampleLine :: Sig s (DeleteEvent :- MouseEv) [OneOfThem (Singleton Line)] ()
sampleLine = do
	(emit . (: []) . Singleton =<<) . waitFor $ Line' (Color 0 0 0) 2
		<$> adjust (maybeEither (0, 0) <$> mousePos `at` leftClick)
		<*> adjust (maybeEither (0, 0) <$> mousePos `at` leftUp)
	waitFor $ adjust deleteEvent

runDraw :: Monoid a => GtkDrawer a -> Sig s (DeleteEvent :- MouseEv) a r -> IO r
runDraw dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	interpret (retry $ handle Nothing cr c) c' s <* gtkMainQuit
