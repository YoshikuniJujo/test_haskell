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
	putFloatList "tmp.wav" . checkFloat . Sound.soundWhole . keyLogToChangers 0 =<< readKeyActions fp

checkFloat :: [Float] -> [Float]
checkFloat [] = []
checkFloat (f : fs)
	| f < - 1 || f > 1 = error $ "bad: " ++ show f
	| otherwise = f : checkFloat fs
