{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import StbImage

main :: IO ()
main = do
	print =<< stbiLoad "../../files/images/texture.jpg" StbiRgbAlpha
