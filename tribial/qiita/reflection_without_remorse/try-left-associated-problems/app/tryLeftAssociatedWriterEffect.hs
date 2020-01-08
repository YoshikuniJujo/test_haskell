{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Eff
import Writer
import LeftAssociatedWriterEffect

main :: IO ()
main = do
	print $ {-# SCC "LEFT" #-} last' . snd . run . runWriter $ sampleL ()
	print $ {-# SCC "RIGHT" #-} last' . snd . run . runWriter $ sampleR ()
