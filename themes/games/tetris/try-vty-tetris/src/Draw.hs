{-# LANGUAGE BlockArguments, TupleSections #-}
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
		translate 30 5 $ string defAttr (show $ score st),
		drawLand' $ mkBody st,
		field
		]
	where (x, y) = position st

mkBody :: State -> M.Map (Int, Int) (Color, Bool)
mkBody st = foldr (\p -> M.insert p (shapeColor st, False))
	(foldr (\p -> M.insert p (shapeColor st, True))
		(M.map (, False) $ foldr (\p -> M.insert p $ shapeColor st) (land st) (blocks st))
		(blocks $ moveBottom st))
		(blocks st)

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
	foldl1 (<|>) (replicate 12 blockW) <|> space <->
	foldl1 (<->) (replicate 23 $ blockW <|> foldl1 (<|>) (replicate 10 space) <|> blockW <|> space) <->
	foldl1 (<|>) (replicate 12 blockW) <|> space

drawLand' :: M.Map (Int, Int) (Color, Bool) -> Image
drawLand' l = translate 6 1 $ foldl1 (<->) $ flip map [0 .. 22] \y -> foldl1 (<|>) $ flip map [0 .. 9] \x ->
	maybe space (\(c, g) -> bool block ghost g c) $ M.lookup (x, y) l

drawLand :: M.Map (Int, Int) Color -> Image
drawLand l = translate 6 1 $ foldl1 (<->) $ flip map [0 .. 22] \y -> foldl1 (<|>) $ flip map [0 .. 9] \x ->
	maybe space block $ M.lookup (x, y) l

drawGhost :: M.Map (Int, Int) Color -> Image
drawGhost l = translate 6 1 $ foldl1 (<->) $ flip map [0 .. 22] \y -> foldl1 (<|>) $ flip map [0 .. 9] \x ->
	maybe space ghost $ M.lookup (x, y) l

ghost :: Color -> Image
ghost c = string (defAttr `withForeColor` c) "##"
