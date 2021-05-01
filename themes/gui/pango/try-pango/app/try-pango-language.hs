{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage

main :: IO ()
main = do
	args <- getArgs
	putStrLn . pangoLanguageToString =<< pangoLanguageGetDefault
	let	l = case args of
			l' : _ -> l'
			_ -> "zh-tw"
	putStrLn . pangoLanguageToString $ pangoLanguageFromString l
	putStrLn . pangoLanguageGetSampleString =<< pangoLanguageGetDefault
	putStrLn . pangoLanguageGetSampleString $ pangoLanguageFromString l
	print $ pangoScriptForUnichar '愛'
