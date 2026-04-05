{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.PortAudio

main :: IO ()
main = withPortAudio do
	print =<< getDevices
