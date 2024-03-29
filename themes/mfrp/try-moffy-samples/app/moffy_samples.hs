{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import Control.Moffy.Samples.Boxes
import Control.Moffy.Samples.Boxes.Run.Gtk4
import Control.Moffy.Samples.Followbox
import Control.Moffy.Samples.FollowboxAnother qualified as A
import Control.Moffy.Samples.FollowboxOrigin qualified as O
import Control.Moffy.Samples.Followbox.Run.Gtk4

main :: IO ()
main = getArgs >>= \case
	["boxes"] -> runBoxes boxes
	["followbox"] -> runFollowbox "firefox" followbox
	["followbox-another"] -> runFollowbox' "firefox" A.followbox
	["followbox-origin"] -> runFollowbox "firefox" O.followbox
	_ -> putStrLn $
		"Usage: moffy_samples [SUBCOMMAND]\n\n" ++
		"Commands are\n\tboxes\n\tfollowbox\n" ++
		"\tfollowbox-another\n\tfollowbox-origin"
