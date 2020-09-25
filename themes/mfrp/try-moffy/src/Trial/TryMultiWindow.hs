{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryMultiWindow where

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.GtkField
import Control.Moffy.Run.TChan
import Data.Type.Set
import Data.Type.Flip

threeWindows :: Sig s (DeleteEvent :- WindowEv) () (WindowId, WindowId, WindowId)
threeWindows = do
	w0 <- waitFor $ adjust windowNew
	w1 <- waitFor $ adjust windowNew
	w2 <- waitFor $ adjust windowNew
	waitFor . find (\(x, y, z) -> not $ x || y || z) $ (,,) <$%> ddw w0 <*%> ddw w1 <*%> ddw w2
	pure (w0, w1, w2)

ddw :: WindowId -> Sig s (DeleteEvent :- WindowEv) Bool ()
ddw i = do
	emit True
	waitFor do
		adjust $ deleteEvent i
		adjust $ windowDestroy i
	emit False
	waitFor never

runTryMultiWindow :: Adjustable es GuiEv => Sig s es () r -> IO r
runTryMultiWindow s = do
	([], (cr, c, c')) <- runGtkMain (\_ _ _ -> pure ()) []
	interpret (retry $ handle Nothing cr c) c' s
