{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Vty

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	let	line0 = string (defAttr `withForeColor` green) "first line"
		line1 = string (defAttr `withBackColor` blue) "second line"
		i = pad 0 1 0 0 $ foldr1 (<|>) (replicate 4 block) <|> space
		o = block <|> block <|> space <-> block <|> block <|> space
		s = space <|> block <|> block <|> space <-> block <|> block <|> space
		z = block <|> block <|> space <-> space <|> block <|> block <|> space
		j = block <|> space <-> foldl1 (<|>) (replicate 3 block) <|> space
		l = space <|> space <|> block <|> space <-> foldl1 (<|>) (replicate 3 block) <|> space 
		t = resize 6 2 $ space <|> block <|> space <-> block <|> block <|> block <|> space
	update vty . picForImage $ line0 <-> line1 <->
		pad 2 1 0 0 i <|> pad 0 1 0 0 o <|> pad 0 1 0 0 s <|> pad 0 1 0 0 z <|>
		pad 0 1 0 0 j <|> pad 0 1 0 0 l <|> pad 0 1 0 0 t
	e <- nextEvent vty
	shutdown vty
	print $ "Last event was: " ++ show e

block :: Image
block = string (defAttr `withBackColor` white) "  "

space :: Image	
space = string (defAttr `withBackColor` black) "  "
