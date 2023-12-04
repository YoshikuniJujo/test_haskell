{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import Control.Moffy.Samples.Boxes
import Control.Moffy.Samples.Boxes.Run
import Control.Moffy.Samples.Followbox
import Control.Moffy.Samples.Followbox.RunGtkField

main :: IO ()
main = getArgs >>= \case
	["boxes"] -> runBoxes boxes
	["followbox"] -> runFollowbox "firefox" followbox
	_ -> putStrLn "Usage: moffy_samples [SUBCOMMAND]\n\nCommands are\n\tboxes\n\tfollowbox"
