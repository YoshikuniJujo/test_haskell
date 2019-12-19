{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Vty

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	let	line0 = string (defAttr `withForeColor` green) "first line"
		line1 = string (defAttr `withBackColor` blue) "second line"
		i = pad 0 1 0 0 $ foldr1 (<|>) (replicate 4 blockC) <|> space
		o = blockY <|> blockY <|> space <-> blockY <|> blockY <|> space
		s = space <|> blockG <|> blockG <|> space <-> blockG <|> blockG <|> space
		z = blockR <|> blockR <|> space <-> space <|> blockR <|> blockR <|> space
		j = blockB <|> space <-> foldl1 (<|>) (replicate 3 blockB) <|> space
		l = space <|> space <|> blockO <|> space <-> foldl1 (<|>) (replicate 3 blockO) <|> space 
		t = resize 6 2 $ space <|> blockM <|> space <-> blockM <|> blockM <|> blockM <|> space
	update vty . picForImage $ line0 <-> line1 <->
		pad 2 1 0 0 i <|> pad 0 1 0 0 o <|> pad 0 1 0 0 s <|> pad 0 1 0 0 z <|>
		pad 0 1 0 0 j <|> pad 0 1 0 0 l <|> pad 0 1 0 0 t
	e <- nextEvent vty
	shutdown vty
	print $ "Last event was: " ++ show e

blockW, blockC, blockY, blockG, blockR, blockB, blockM, blockO :: Image
[blockW, blockC, blockY, blockG, blockR, blockB, blockM, blockO] =
	block <$> [white, cyan, brightYellow, green, red, blue, magenta, white]

block :: Color -> Image
block c = string (defAttr `withBackColor` c) "  "

space :: Image	
space = string (defAttr `withBackColor` black) "  "
