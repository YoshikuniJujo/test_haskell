{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Vty

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	let	t = space <|> blockM <|> space <-> foldl1 (<|>) (replicate 3 blockM)
	update vty . picForImage $ pad 1 1 0 0 t
	e <- nextEvent vty
	shutdown vty
	print $ "Last event was: " ++ show e

blockM :: Image
[blockM] = block <$> [magenta]

block :: Color -> Image
block c = string (defAttr `withBackColor` c) "  "

space :: Image
space = string (defAttr `withBackColor` black) "  "
