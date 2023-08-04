{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import qualified Data.ByteString as BS

import qualified Codec.WavefrontObj.ReadSimple as New

main :: IO ()
main = do
	[objfile] <- getArgs
	obj <- BS.readFile objfile
	let	New.Result vs _ts ns fs = New.r obj
	print $ New.facePosNormal vs ns fs
