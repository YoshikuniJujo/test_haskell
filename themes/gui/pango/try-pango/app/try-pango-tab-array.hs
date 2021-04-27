{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Pango.Basic.TabStops

main :: IO ()
main = do
	tad <- pangoTabArrayDoubleNew
	pangoTabArrayDoubleSetTab tad 3 100
	pangoTabArrayDoubleSetTab tad 5 250
	print $ unsafeTryTabArrayDoubleGetTabs tad

	tai <- pangoTabArrayIntNew
	pangoTabArrayIntSetTab tai 10 100
	print $ unsafeTryTabArrayIntGetTabs tai
