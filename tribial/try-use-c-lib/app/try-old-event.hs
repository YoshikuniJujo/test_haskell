{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad

import Human.EventOld

main :: IO ()
main = replicateM_ 10 do
	replicateM_ 10 getEvent
	print =<< replicateM 10 getEvent
