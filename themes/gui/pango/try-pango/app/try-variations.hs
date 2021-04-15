{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations

main :: IO ()
main = do
	let	v = readVariations "foom=80.55,barz=32"
	print v
	print $ showVariations v
