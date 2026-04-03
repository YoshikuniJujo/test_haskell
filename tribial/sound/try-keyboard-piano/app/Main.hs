{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Concurrent.STM
import Data.Function
import Graphics.UI.GLFW qualified as Glfw

import KeyLog

main :: IO ()
main = do
	end <- atomically $ newTVar False
	kl <- atomically $ newTVar []
	print =<< Glfw.init
	Just win <- Glfw.createWindow 100 100 "Hello" Nothing Nothing
	Glfw.setKeyCallback win $ Just \w k n ks mks -> do
		Just t <- Glfw.getTime
		print t
		print (w, k, n, ks, mks)
		atomically $ modifyTVar kl (++ [KeyLog t k ks])
		case k of
			Glfw.Key'Q -> atomically $ writeTVar end True
			_ -> pure ()
	fix \go -> do
		Glfw.waitEvents
		e <- atomically $ readTVar end
		when (not e) go
	Glfw.terminate
	writeKeyLog "foo.keys"
		. filter ((/= Glfw.Key'Q) . klKey)
		. filter ((/= Glfw.KeyState'Repeating) . klAction) =<< atomically (readTVar kl)
