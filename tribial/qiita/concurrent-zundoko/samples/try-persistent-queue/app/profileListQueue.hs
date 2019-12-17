{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import ProfileQueue
import ListQueue

main :: IO ()
main = profileWhole @ListQueue 1000
