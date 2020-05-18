{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Trials.Followbox (followbox)
import Trials.Followbox.Run (runFollowbox)

main :: IO ()
main = () <$ runFollowbox "Followbox" followbox
