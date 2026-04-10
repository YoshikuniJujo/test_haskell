{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import System.Environment

import Graphics.UI.GLFW qualified as Glfw

import KeyEvent
import KeySound
import Sound qualified as Sound
import WriteMonoral16

main :: IO ()
main = do
	fp : _ <- getArgs
	putFloatList "tmp.wav" . Sound.soundWhole . keyLogToChangers 0 =<< readKeyActions fp
