{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import qualified Data.ByteString as BS

import Codec.Wavefront.ReadOld

main :: IO ()
main = do
	[objfile] <- getArgs
	obj <- BS.readFile objfile
	let	Count { countVertex = cv, countNormal = cn, countFace = cf } =
			countV' obj
		(vs, ns, fs) = readV' cv cn cf obj
	print $ facePosNormal vs ns fs
