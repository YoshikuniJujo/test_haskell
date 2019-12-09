{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import ProfileQueue
import BatchedQueue

main :: IO ()
main = profileWhole @BatchedQueue 100000
