{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryReact where

import React
import Handlers
import Field

tryLeftClick :: IO ()
tryLeftClick = do
	f <- openField "tryLeftClick" [exposureMask, buttonPressMask]
	interpret (handleWithoutTime f) (adjust leftClick) >>= print
	closeField f

trySameClick :: IO ()
trySameClick = do
	f <- openField "trySameClick" [exposureMask, buttonPressMask]
	interpret (handleWithoutTime f) sameClick >>= print
	closeField f
