{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Vty

-- "ijlostz"

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	let	i = pad 0 1 0 0 $ foldr1 (<|>) (replicate 4 blockR) <|> space
		j = blockG <|> space <-> foldl1 (<|>) (replicate 3 blockG) <|> space
		l = space <|> space <|> blockY  <|> space <-> foldl1 (<|>) (replicate 3 blockY) <|> space
		o = blockB <|> blockB <|> space <-> blockB <|> blockB <|> space
		s = space <|> blockM <|> blockM <|> space <-> blockM <|> blockM <|> space
		t = space <|> blockC <|> space <-> foldl1 (<|>) (replicate 3 blockC) <|> space
		z = blockW <|> blockW <|> space <-> space <|> blockW <|> blockW <|> space
	update vty . picForImage $
		pad 1 1 0 0 i <|> pad 1 1 0 0 j <|> pad 1 1 0 0 l <|>
		pad 1 1 0 0 o <|>
		pad 1 1 0 0 s <|> pad 1 1 0 0 t <|> pad 1 1 0 0 z
	e <- nextEvent vty
	shutdown vty
	print $ "Last event was: " ++ show e

blockR, blockG, blockY, blockB, blockM, blockC, blockW :: Image
[blockR, blockG, blockY, blockB, blockM, blockC, blockW] =
	block <$> [red, green, yellow, blue, magenta, cyan, white]

block :: Color -> Image
block c = string (defAttr `withBackColor` c) "  "

space :: Image
space = string (defAttr `withBackColor` black) "  "
