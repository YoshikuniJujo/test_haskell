module Main where

import Trials.Followbox
import Trials.Followbox.Run

main :: IO ()
main = () <$ runFollowbox "Followbox" followbox
