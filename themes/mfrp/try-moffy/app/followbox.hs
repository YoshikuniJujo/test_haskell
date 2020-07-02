{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Trial.Followbox (followbox)
import Trial.Followbox.Run (runFollowbox)

main :: IO ()
main = () <$ runFollowbox "Followbox" followbox
