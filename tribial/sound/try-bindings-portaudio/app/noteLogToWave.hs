{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import Keyboard
import KeyboardSound
import Sound qualified
import WriteMonoral16

main :: IO ()
main = do
	fp : _ <- getArgs
	ne <- readNoteEvent fp
	putFloatList "tmp.wav" . Sound.soundWhole $ noteToChangers (fst $ head ne) ne
