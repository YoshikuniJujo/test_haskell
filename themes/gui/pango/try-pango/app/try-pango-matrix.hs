{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Pango.Basic.GlyphStorage.PangoMatrix

main :: IO ()
main = do
	let	m0 = PangoMatrix 1 2 3 4 5 6
	print m0
	m1 <- pangoMatrixThaw m0
	print m1
	m2 <- pangoMatrixCopy m1
	print m2
	pangoMatrixTranslate m2 100 200
	m3 <- pangoMatrixFreeze m2
	print m3
