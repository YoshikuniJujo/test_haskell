{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Trial.Followbox (followbox)
import Trial.Followbox.Run (evalFollowbox)

main :: IO ()
main = evalFollowbox "FOLLOW BOX" followbox
