{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KeyEvent (
	withKeyActions, KeyActions, KeyAction(..), Action(..) ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Function
import Graphics.UI.GLFW qualified as Glfw

withKeyActions :: Int -> Int -> String -> IO (TVar KeyActions)
withKeyActions wdt hgt ttl = do
	vkas <- newKeyActions
	(print =<<) . forkIO . glfwWith $ glfwWithWindow wdt hgt ttl \win -> do
			Glfw.setKeyCallback win $ Just \_w k _n ks _mks -> do
				Just t <- Glfw.getTime
				maybe (pure ()) (putKeyAction vkas . mkKeyAction t k) $ keyStateToAction ks

			fix \go -> threadDelay 100000 >> Glfw.waitEvents >> go

	pure vkas

glfwWith :: IO a -> IO a
glfwWith = bracket_
	do	b <- Glfw.init
		when (not b) $ error "Cannot initialize GLFW"
	Glfw.terminate

glfwWithWindow :: Int -> Int -> String -> (Glfw.Window -> IO a) -> IO a
glfwWithWindow wdt hgt ttl = bracket
	do	mw <- Glfw.createWindow wdt hgt ttl Nothing Nothing
		maybe (error "Cannot create window") pure mw
	Glfw.destroyWindow

newKeyActions :: IO (TVar KeyActions)
newKeyActions = atomically $ newTVar []

putKeyAction :: TVar KeyActions -> KeyAction -> IO ()
putKeyAction vkas ka = atomically do
	kas <- readTVar vkas
	writeTVar vkas $ kas ++ [ka]

mkKeyAction :: Double -> Glfw.Key -> Action -> KeyAction
mkKeyAction t k a = KeyAction {
	keyActionTime = t,
	keyActionKey = k,
	keyActionAction = a }

data KeyAction = KeyAction {
	keyActionTime :: Double,
	keyActionKey :: Glfw.Key,
	keyActionAction :: Action }
	deriving Show

data Action = Press | Release deriving Show

keyStateToAction :: Glfw.KeyState -> Maybe Action
keyStateToAction Glfw.KeyState'Pressed = Just Press
keyStateToAction Glfw.KeyState'Released = Just Release
keyStateToAction _ = Nothing

type KeyActions = [KeyAction]
