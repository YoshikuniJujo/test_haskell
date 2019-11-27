{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

kmpTable :: Eq a => [a] -> [Int]
kmpTable pat = kt pat
	where
	kt jjjj:
