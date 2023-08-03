{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import qualified Data.ByteString as BS

import qualified Codec.WavefrontObj.Read as New

main :: IO ()
main = do
	[objfile] <- getArgs
	obj <- BS.readFile objfile
	let	(vs, ns, fs) = New.readPosNormal (New.countV obj) obj
	print $ New.facePosNormal vs ns fs
