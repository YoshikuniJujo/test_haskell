{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Draw where

import Data.Foldable
import Data.Bool
import Graphics.Vty

import qualified Data.Map as M

import State

draw :: Vty -> State -> IO ()
draw vty st = do
	update vty $ picForLayers [
		translate (x * 2 + 6) (y + 1) minoT,
		drawLand $ land st,
		field
		]
	where (x, y) = position st

blockR, blockG, blockY, blockB, blockM, blockC, blockW :: Image
[blockR, blockG, blockY, blockB, blockM, blockC, blockW] =
	block <$> [red, green, yellow, blue, magenta, cyan, white]

minoT :: Image
minoT = space <|> blockC <|> space <-> foldl1 (<|>) (replicate 3 blockC)

block :: Color -> Image
block c = string (defAttr `withBackColor` c) "  "

space :: Image
space = block black

field :: Image
field = translate 4 0 $
	foldl1 (<|>) (replicate 12 blockW) <->
	foldl1 (<->) (replicate 23 $ blockW <|> foldl1 (<|>) (replicate 10 space) <|> blockW) <->
	foldl1 (<|>) (replicate 12 blockW)

drawLand :: M.Map (Int, Int) Color -> Image
drawLand l = translate 6 1 $ foldl1 (<->) $ flip map [0 .. 22] \y -> foldl1 (<|>) $ flip map [0 .. 9] \x ->
	bool space blockC (M.lookup (x, y) l == Just cyan)
