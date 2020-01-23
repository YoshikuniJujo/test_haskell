{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

main :: IO ()
main = do
	eq <- (<$> getArgs) \case "-p" : _ -> (==); _ -> (/=)
	interact $ unlines . filter (eq 's' . last) . lines
