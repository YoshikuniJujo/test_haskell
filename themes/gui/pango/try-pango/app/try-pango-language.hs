{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
	let	
		pl = PangoLanguage l
		s = pangoScriptForUnichar '愛'
		s' = pangoScriptForUnichar 'あ'
	print s
	print s'
	print =<< pangoScriptGetSampleLanguage s
	print =<< pangoScriptGetSampleLanguage s'
	print $ pangoScriptForText "愛 love 友"
	print @PangoLanguage $ read "PangoLanguage \"ja-JP\""
	print @(Maybe PangoLanguage) $ read "Just (PangoLanguage \"zh-TW\")"
	print $ pangoLanguageMatches pl "en;ja-jp"
	print $ pangoLanguageIncludesScript pl PangoScriptHan
	print $ pangoLanguageGetScripts pl
