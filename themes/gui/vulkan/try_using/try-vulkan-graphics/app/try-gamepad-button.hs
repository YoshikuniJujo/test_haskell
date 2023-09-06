{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import Data.Bool

import Graphics.UI.GLFW qualified as GLFW

main :: IO ()
main = GLFW.init >> doWhile_ do
	threadDelay 100000
	Just (GLFW.GamepadState gb ga) <- GLFW.getGamepadState GLFW.Joystick'1
	print $ ga GLFW.GamepadAxis'LeftX
	print $ ga GLFW.GamepadAxis'LeftY
	pure $ gb GLFW.GamepadButton'A /= GLFW.GamepadButtonState'Pressed

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act
