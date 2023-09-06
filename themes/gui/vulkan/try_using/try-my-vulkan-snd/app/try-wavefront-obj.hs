{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import qualified Data.ByteString as BS

import qualified Codec.WavefrontObj.ReadFaceSimple as New

main :: IO ()
main = do
	[objfile] <- getArgs
	obj <- BS.readFile objfile
	print . New.posNormal $ New.r obj
