{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryReact where

import React
import Handlers
import Field

tryLeftClick :: IO ()
tryLeftClick = do
	f <- openField "tryLeftClick" [exposureMask, buttonPressMask]
	interpret (handleWithoutTime f) leftClick >>= print
	closeField f
