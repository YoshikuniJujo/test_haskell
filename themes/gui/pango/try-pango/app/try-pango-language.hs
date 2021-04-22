{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage

main :: IO ()
main = do
	putStrLn . pangoLanguageToString =<< pangoLanguageGetDefault
