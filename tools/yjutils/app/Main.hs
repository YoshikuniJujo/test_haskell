{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import qualified Tr

main :: IO ()
main = getArgs >>= \case
	["tr", "-r", src, dst, fp] -> do
		Tr.rec src dst fp
	_ -> error "no such command"
