{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Trial.Followbox (followbox)
import Trial.Followbox.RunXField (evalFollowbox)

main :: IO ()
main = evalFollowbox "FOLLOW BOX" followbox
