{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.TryReact where

import MonadicFrp.MyInterface
import TryMyInterface.Boxes
import TryMyInterface.Boxes.Events
import TryMyInterface.Boxes.Handlers
import Field

tryLeftClick :: IO ()
tryLeftClick = do
	f <- openField "tryLeftClick" [exposureMask, buttonPressMask]
	interpret (handleWithoutTime f) (adjust leftClick :: ReactG ()) >>= print
	closeField f
