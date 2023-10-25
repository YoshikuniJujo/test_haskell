{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import System.Directory
import System.FilePath
import System.IO.Temp

main :: IO ()
main = do
	src : dst : fps <- getArgs
	translateFile src dst `mapM_` fps

translateFile :: String -> String -> FilePath -> IO ()
translateFile src dst fp = do
	cnt <- readFile fp
	fp' <- writeSystemTempFile (takeFileName fp)
		$ translate (escape src) (escape dst) cnt
	putStrLn fp'
	copyFile fp' fp

escape :: String -> String
escape "" = ""
escape ('\\' : 'n' : cs) = '\n' : escape cs
escape (c : cs) = c : escape cs

translate :: String -> String -> String -> String
translate src dst = \case
	"" -> ""
	str@(c : cs)
		| s == src -> dst ++ translate src dst t
		| otherwise -> c : translate src dst cs
		where (s, t) = splitAt (length src) str
