{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoScript
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage
import Graphics.Pango.Basic.ScriptsAndLanguages.Types

main :: IO ()
main = do
	args <- getArgs
	dl <- pangoLanguageGetDefault
	print dl
	putStrLn $ getPangoLanguage dl
	putStrLn case dl of PangoLanguage s -> s
	let	l = case args of
			l' : _ -> l'
			_ -> "zh-tw"
	print . Just =<< pangoLanguageGetDefault
	putStrLn . getPangoLanguage $ PangoLanguage l
	putStrLn . getPangoLanguage =<< pangoLanguageGetDefault
	putStrLn . getPangoLanguage $ PangoLanguage l
	print $ pangoScriptForUnichar '愛'
