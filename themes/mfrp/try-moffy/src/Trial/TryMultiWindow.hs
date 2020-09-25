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

threeWindows :: React s (DeleteEvent :- WindowNew :- 'Nil) (WindowId, WindowId, WindowId, WindowId)
threeWindows =
	(,,,) <$> adjust windowNew <*> adjust windowNew <*> adjust windowNew <*> adjust deleteEvent

runTryMultiWindow :: Adjustable es GuiEv => Sig s es () r -> IO r
runTryMultiWindow s = do
	([], (cr, c, c')) <- runGtkMain (\_ _ _ -> pure ()) []
	interpret (retry $ handle Nothing cr c) c' s
