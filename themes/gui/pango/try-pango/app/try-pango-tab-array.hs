{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Pango.Basic.TabStops

main :: IO ()
main = do
	print =<< pangoTabArrayDoubleNew
	print =<< pangoTabArrayIntNew
