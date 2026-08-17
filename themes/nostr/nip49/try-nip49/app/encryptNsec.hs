{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
	let	pss = case pssfp of
			"-" -> withNoEcho getLine; _ -> chomp <$> readFile pssfp
	T.writeFile cnsfp =<< Ncryptsec.fromNsec 16 0 pss ns

chomp :: String -> String
chomp = reverse . go . reverse
	where go = \case '\n' : '\r' : r -> r; '\n' : r -> r; r -> r
