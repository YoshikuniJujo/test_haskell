{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import qualified Data.ByteString as BS

import qualified Codec.WavefrontObj.ReadFaceSimple as WfRd

main :: IO ()
main = do
	[objfile] <- getArgs
	obj <- BS.readFile objfile
	print . WfRd.posNormal $ WfRd.r obj
