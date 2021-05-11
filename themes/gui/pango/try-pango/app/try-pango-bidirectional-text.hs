{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Pango.Basic.BidirectionalText

main :: IO ()
main = do
	print $ pangoUnicharDirection 'c'
	print $ pangoUnicharDirection 'あ'
	print $ pangoUnicharDirection '\x0644'
	print $ pangoFindBaseDir "Hello, حروف شمسية"
	print $ pangoFindBaseDir " حروف شمسية あいうえお"
	print $ pangoBidiTypeForUnichar 'あ'
	print $ pangoBidiTypeForUnichar '\x0644'
