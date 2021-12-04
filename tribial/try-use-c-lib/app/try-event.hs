{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.IO

import Human.Event

main :: IO ()
main = do
	ch <- hGetAndPushCChar stdin
	forever $ withEvent ch (const $ pure ())
