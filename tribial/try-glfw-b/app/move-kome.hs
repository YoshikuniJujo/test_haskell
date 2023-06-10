{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import Data.Bool

import Graphics.UI.GLFW qualified as GLFW

main :: IO ()
main = GLFW.init >> doWhileWithState_ (25, 50) \st -> do
	threadDelay 100000
	Just (GLFW.GamepadState gb ga) <- GLFW.getGamepadState GLFW.Joystick'1
	let	dx = ga GLFW.GamepadAxis'LeftX
		dy = ga GLFW.GamepadAxis'LeftY
	putStr $ replicate (round $ fst st) '\n'
	putStrLn $ replicate (round $ snd st) ' ' ++ "*"
	putStr $ replicate (50 - round (fst st)) '\n'
	let	b = gb GLFW.GamepadButton'A /= GLFW.GamepadButtonState'Pressed
	pure $ bool Nothing (Just $ (fst st + dy * 5, snd st + dx * 5)) b

doWhileWithState_ :: Monad m => st -> (st -> m (Maybe st)) -> m ()
doWhileWithState_ st act = do
	r <- act st
	case r of
		Nothing -> pure ()
		Just st' -> doWhileWithState_ st' act
