{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryLifegamePattern where

import Data.Lifegame.Glider
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Lifegame.Words

import System.File.Png.Lifegame

pattern1 :: Pattern
pattern1 = asciiToPattern 3 3 0 0 shape1

board1 :: Board
board1 = unsafePerformIO do
	hm <- getHomeDirectory
	readBoard (hm </>
		".yoshj/lifegame/pngs/glider/10x10_3x3/board_0000.png")
