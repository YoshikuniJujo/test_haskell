{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow

import TsFiles

main :: IO ()
main = putStrLn
	. (\(t, j) -> show t ++ "/" ++ show (t + j))
	. (length *** length) =<< tsJsFiles
