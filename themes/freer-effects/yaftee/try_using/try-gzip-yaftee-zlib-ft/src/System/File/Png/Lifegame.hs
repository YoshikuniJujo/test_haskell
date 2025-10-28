{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Png.Lifegame where

import System.File.Png.Gray1.NoInterlace qualified as Png
import Lifegame.Words qualified as Lifegame

writeBoard :: FilePath -> Lifegame.Board -> Int -> IO ()
writeBoard fpo bd n = Png.write fpo (Lifegame.boardToGray1' n bd)
