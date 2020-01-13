{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import CodensityWriterMonad

main :: IO ()
main = do
	print . last . getLog $ sampleLWriter ()
	print . last . getLog $ sampleRWriter ()
