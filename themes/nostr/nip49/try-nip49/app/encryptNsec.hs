{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Text.IO qualified as T
import System.Environment
import System.IO.Yj

import Ncryptsec qualified

main :: IO ()
main = do
	[nsfp, pssfp, cnsfp] <- getArgs
	ns <- T.readFile nsfp
	T.writeFile cnsfp =<< case pssfp of
		"-" -> Ncryptsec.fromNsec 16 0 (withNoEcho getLine) ns
		_ -> Ncryptsec.fromNsec 16 0 (readFile pssfp) ns
