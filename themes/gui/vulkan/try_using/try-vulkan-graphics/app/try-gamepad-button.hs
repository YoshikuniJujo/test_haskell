{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import Data.Bool
import Graphics.UI.GLFW qualified as GLFW

main :: IO ()
main = GLFW.init >> doWhile_ do
	threadDelay 50000
	Just (GLFW.GamepadState gb ga) <- GLFW.getGamepadState GLFW.Joystick'1
	let	(x, y) = (ga GLFW.GamepadAxis'LeftX, ga GLFW.GamepadAxis'LeftY)
	print x; print y; putStrLn $ bar x; putStrLn $ bar y
	putStr $ dot y x
	pure $ gb GLFW.GamepadButton'A /= GLFW.GamepadButtonState'Pressed

bar :: Float -> String
bar n = replicate (40 + round (n * 40)) '*'

dot :: Float -> Float -> String
dot y x =
	replicate (15 + round (y * 15)) '\n' ++
	replicate (30 + round (x * 30)) ' ' ++ "*\n" ++
	replicate (14 - round (y * 15)) '\n'

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ a = bool (pure ()) (doWhile_ a) =<< a
