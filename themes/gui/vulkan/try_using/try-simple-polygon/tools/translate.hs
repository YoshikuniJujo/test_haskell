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
	changeFile a b `mapM_` fps

translate :: String -> String -> String -> String
translate _ _ "" = ""
translate (prep -> a) (prep -> b) s@(c : t)
	| a `isPrefixOf` s = b ++ translate a b (drop (length a) s)
	| otherwise = c : translate a b t

prep :: String -> String
prep "" = ""
prep ('\\' : '\\' : cs) = '\\' : prep cs
prep ('\\' : 't' : cs) = '\t' : prep cs
prep ('\\' : 'n' : cs) = '\n' : prep cs
prep (c : cs) = c : prep cs

changeFile :: String -> String -> FilePath -> IO ()
changeFile a b fp = do
	withSystemTempFile "translate" \tfp th -> do
		putStrLn tfp
		cnt <- readFile fp
		hPutStr th (translate a b cnt)
		hClose th
		copyFile tfp fp
