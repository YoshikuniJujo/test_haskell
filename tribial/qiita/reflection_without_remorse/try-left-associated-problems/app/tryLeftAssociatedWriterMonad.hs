{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import LeftAssociatedWriterMonad

main :: IO ()
main = do
	print . last . getLog $ sampleL ()
	print . last . getLog $ sampleR ()
