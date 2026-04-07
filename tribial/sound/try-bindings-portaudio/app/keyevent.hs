{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable
import Data.Function
import Graphics.UI.GLFW qualified as Glfw

import KeyEvent

main :: IO ()
main = do
	end <- atomically $ newTVar False
	vkas <- withKeyActions 100 100 "Hello"
	fix \go -> do
		threadDelay 100000
		kas <- atomically $ readTVar vkas <* writeTVar vkas []
		for_ kas \ka -> do
			print ka
			atomically . when (keyActionKey ka == Glfw.Key'Q)
				$ writeTVar end True
		(`when` go) . not =<< atomically (readTVar end)
