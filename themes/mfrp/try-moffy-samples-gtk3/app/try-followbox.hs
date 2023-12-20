module Main where

import Control.Moffy.Samples.FollowboxOrigin
import Control.Moffy.Samples.Followbox.Run.Gtk3

main :: IO ()
main = runFollowbox "" followbox
