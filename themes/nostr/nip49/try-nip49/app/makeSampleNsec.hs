{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Text.IO qualified as T
import Data.Hex qualified as Hex
import System.Environment

import Codec.Bech32 qualified as Bech32
import Tools

main :: IO ()
main = do
	[fp, bn] <- getArgs
	hs <- Hex.readFileList fp
	let	bs = zip (fileNameN bn "nsec" <$> [0 ..])
			$ Bech32.encode . Bech32.B "nsec" . Hex.unH <$> hs
	uncurry T.writeFile `mapM_` bs
