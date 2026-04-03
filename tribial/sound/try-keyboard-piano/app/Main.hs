{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Concurrent.STM
import Data.Function
import Graphics.UI.GLFW qualified as Glfw

main :: IO ()
main = do
	end <- atomically $ newTVar False
	print =<< Glfw.init
	Just win <- Glfw.createWindow 100 100 "Hello" Nothing Nothing
	Glfw.setKeyCallback win $ Just \w k n ks mks -> do
		print (w, k, n, ks, mks)
		case k of
			Glfw.Key'Q -> atomically $ writeTVar end True
			_ -> pure ()
	fix \go -> do
		Glfw.waitEvents
		e <- atomically $ readTVar end
		when (not e) go
