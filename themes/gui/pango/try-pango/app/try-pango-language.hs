{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage

main :: IO ()
main = do
	args <- getArgs
	putStrLn . pangoLanguageToString =<< pangoLanguageGetDefault
	let	l = case args of
			l' : _ -> l'
			_ -> "ch-zw"
	putStrLn . pangoLanguageToString $ pangoLanguageFromString l
