{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryScroll where

import Prelude hiding (repeat, scanl)

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Run.TChan
import Trial.Draw.Viewable
import Data.Type.Set
import Data.Type.Flip

import Control.Moffy.Run.GtkField
import Graphics.Gtk

import Data.Map
import Control.Moffy.Event.Window

tryScroll :: Sig s (LoadDefaultWindow :- MouseScroll :- 'Nil) [Message] ()
tryScroll = (: []) . Message . show <$%> scanl addPoints (0, 0) (repeat mouseScroll)

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

runTryScroll :: (Monoid a, Adjustable es (DefaultWindowEv :+: GuiEv)) => GtkDrawer' a -> Sig s es (Map WindowId a) r -> IO (r, Maybe WindowId)
runTryScroll dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	r <- interpretSt (retrySt $ handleDefaultWindow `mergeSt` liftHandle' (handle Nothing cr c)) c' do
			s
		Nothing
	gtkMainQuit
	pure r
