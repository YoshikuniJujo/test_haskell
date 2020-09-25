{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryDefaultWindow where

import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Handle
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Run

tryDefaultWindow :: Sig s DefaultWindowEv WindowId ()
tryDefaultWindow = do
--	emit =<< waitFor (adjust loadDefaultWindow)
	waitFor . adjust . storeDefaultWindow $ WindowId 123
	emit =<< waitFor (adjust loadDefaultWindow)
	waitFor . adjust . storeDefaultWindow $ WindowId 456
	emit =<< waitFor (adjust loadDefaultWindow)
	waitFor . adjust . storeDefaultWindow $ WindowId 789
	emit =<< waitFor (adjust loadDefaultWindow)

runTryDefaultWindow :: Sig s DefaultWindowEv WindowId r -> IO (r, Maybe WindowId)
runTryDefaultWindow s = interpretSt (retrySt handleDefaultWindow) print s Nothing
