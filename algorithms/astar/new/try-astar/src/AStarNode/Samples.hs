{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStarNode.Samples where

import Data.Word

import AStarNode

data Pos = Pos Word8 Word8 deriving (Show, Eq, Ord)

instance GlaphNode Pos where
	startNode = Pos 1 1
	isEndNode (Pos 3 3) = True
	isEndNode _ = False
	nextNodes (Pos x y) = (, 1) <$> filter ((&&) <$> walkable <*> inside) (uncurry Pos <$> [
				(x, y + 1),
		(x - 1, y),			(x + 1, y),
				(x, y - 1) ])

	distToEnd (Pos x y) = fromIntegral $ 4 - x + 4 - y

inside :: Pos -> Bool
inside (Pos x y) = 0 <= x && x <= 4 && 0 <= y && y <= 4

walkable :: Pos -> Bool
walkable (Pos 2 3) = False
walkable (Pos 3 2) = False
walkable (Pos 1 2) = False
walkable (Pos 2 1) = False
walkable _ = True
