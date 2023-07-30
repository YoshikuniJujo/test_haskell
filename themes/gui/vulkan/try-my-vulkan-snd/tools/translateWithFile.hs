{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List
import System.IO
import System.IO.Temp
import System.Environment
import System.Directory

main :: IO ()
main = do
	a : b : fps <- getArgs
	a' <- readFile a
	b' <- readFile b
	changeFile a' b' `mapM_` fps

translate :: String -> String -> String -> String
translate _ _ "" = ""
translate a b s@(c : t)
	| a `isPrefixOf` s = b ++ translate a b (drop (length a) s)
	| otherwise = c : translate a b t

changeFile :: String -> String -> FilePath -> IO ()
changeFile a b fp = do
	withSystemTempFile "translate" \tfp th -> do
		putStrLn tfp
		cnt <- readFile fp
		hPutStr th (translate a b cnt)
		hClose th
		copyFile tfp fp
