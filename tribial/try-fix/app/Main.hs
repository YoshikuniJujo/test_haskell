{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Fix

main :: IO ()
main = fix \go -> getLine >>= \case
	"quit" -> pure ()
	ln -> putStrLn ln >> go
