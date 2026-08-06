{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.ByteString qualified as BS
import Data.Hex qualified as Hex
import System.Environment

main :: IO ()
main = do
	[fp] <- getArgs
	print fp
	let	hs = Hex.H . BS.pack <$> [
			[0 .. 31], replicate 32 0, replicate 32 0xff ]
	Hex.writeFileList fp hs
