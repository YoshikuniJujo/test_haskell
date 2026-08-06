{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Text.IO qualified as T
import Data.Hex qualified as Hex
import System.Environment

import Bech32 qualified

main :: IO ()
main = do
	[fp, bn] <- getArgs
	hs <- Hex.readFileList fp
	let	bs = zip ((++ ".nsec") . (bn ++) . showInt2 <$> [0 ..])
			$ Bech32.encode . Bech32.fromByteString "nsec" . Hex.unH <$> hs
	uncurry T.writeFile `mapM_` bs

showInt2 :: Int -> String
showInt2 n = replicate (2 - length s) '0' ++ s where s = show n
