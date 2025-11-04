{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryLifegamePattern where

import Data.Lifegame.Glider
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Lifegame.Words

import System.File.Png.Lifegame

pattern1, pattern2 :: Pattern
pattern1 = asciiToPattern 3 3 0 0 shape1
pattern2 = asciiToPattern 11 11 4 4 shape1

board1, board2 :: Board
board1 = unsafePerformIO do
	hm <- getHomeDirectory
	readBoard (hm </>
		".yoshj/lifegame/pngs/glider/10x10_3x3/board_0000.png")

board2 = unsafePerformIO do
	hm <- getHomeDirectory
	readBoard (hm </>
		".yoshj/lifegame/pngs/glider/10x10_3x3/board_0028.png")

lifegamePngsDirectory :: IO FilePath
lifegamePngsDirectory = do
	hm <- getHomeDirectory
	pure $ hm </> ".yoshj/lifegame/pngs"
