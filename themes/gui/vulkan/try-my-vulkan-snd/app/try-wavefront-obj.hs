{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import qualified Data.ByteString as BS

import qualified Codec.Wavefront.Read as New

main :: IO ()
main = do
	[objfile] <- getArgs
	obj <- BS.readFile objfile
	let	New.Count { New.countVertex = cv, New.countNormal = cn, New.countFace = cf } =
			New.countV obj
		(vs, ns, fs) = New.readVOld cv cn cf obj
	print $ New.facePosNormal vs ns fs
