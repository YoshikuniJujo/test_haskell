{-# LANGUAGE TupleSections, TypeFamilies, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.DiagramMap where

import Prelude as P

import Data.Map.Strict
import Data.Word

data DiagramMap = DiagramMap { width :: Int, height :: Int, layout :: Map Pos ElementDiagram } deriving Show

getWidthDiagramMap, getHeightDiagramMap :: DiagramMap -> Int
getWidthDiagramMap = width
getHeightDiagramMap = height

setWidthDiagramMap, setHeightDiagramMap :: DiagramMap -> Int -> DiagramMap
setWidthDiagramMap d w = d { width = w }
setHeightDiagramMap d h = d { height = h }

data Pos = Pos { posX :: Int, posY :: Int } deriving (Show, Eq, Ord)

mkDiagramMap :: Int -> Int -> DiagramMap
mkDiagramMap w h = DiagramMap { width = w, height = h, layout = empty }

data ElementDiagram
	= Stump
	| AndGateE | OrGateE | NotGateE | TriGateE | ConstGateE Word64
	| DelayE Word8
	| BranchE
	| HLine | VLine
	| TopLeft | TopRight | BottomLeft | BottomRight
	| EndHLine | EndHLineR | EndTopLeft
	| EndBottomLeft
	| TShape | TInverted | TLeft | TRight | CrossDot | Cross
	| HLineText String String
	deriving Show

stump :: ElementDiagram -> Pos -> Map Pos ElementDiagram -> Map Pos ElementDiagram
stump e p m = P.foldr (flip insert Stump) m
	[ Pos x y |
		x <- [x0 .. x0 + w - 1],
		y <- [y0 - h' .. y0 + h''],
		(x, y) /= (x0, y0) ]
	where
	(w, (h', h'')) = elementSpace e
	(x0, y0) = (posX p, posY p)

elementSpace :: ElementDiagram -> (Int, (Int, Int))
elementSpace AndGateE = (3, (1, 1))
elementSpace OrGateE = (3, (1, 1))
elementSpace NotGateE = (2, (1, 1))
elementSpace (ConstGateE _) = (3, (0, 0))
elementSpace TriGateE = (2, (2, 1))
elementSpace (DelayE _) = (2, (0, 0))
elementSpace BranchE = (1, (0, 1))
elementSpace _ = (1, (0, 0))

elementToPositions :: ElementDiagram -> Pos -> [Pos]
elementToPositions e (Pos x0 y0) = [ Pos x y |
	x <- [x0 .. x0 + w - 1],
	y <- [y0 - h .. y0 + h'] ]
	where (w, (h, h')) = elementSpace e

posToLine :: Dir -> [Pos] -> Either String [ElementDiagram]
posToLine _ [] = Right []
posToLine d [_] = (: []) <$> dirToLine' d L
posToLine d (x : xs@(y : _)) = do
	d' <- dir x y; (:) <$> dirToLine d d' <*> posToLine d' xs

insertLine :: [Pos] -> Map Pos ElementDiagram -> Either String (Map Pos ElementDiagram)
insertLine ps m =
	P.foldr (uncurry overlapInsertLine) m . zip ps <$> posToLine L ps

overlapInsertLine :: Pos -> ElementDiagram -> Map Pos ElementDiagram -> Map Pos ElementDiagram
overlapInsertLine pos ln m = case m !? pos of
	Just ln' -> insert pos (overlapLine ln' ln) m
	Nothing -> insert pos ln m

overlapLine :: ElementDiagram -> ElementDiagram -> ElementDiagram
overlapLine HLine EndBottomLeft = TShape
overlapLine EndHLine EndBottomLeft = TShape
overlapLine VLine HLine = Cross
overlapLine VLine EndHLine = TLeft
overlapLine VLine EndHLineR = TRight
overlapLine HLine TopLeft = TInverted
overlapLine BottomRight EndHLineR = TShape
overlapLine BottomRight TopLeft = CrossDot
overlapLine HLine BottomRight = TShape
overlapLine HLine EndTopLeft = TInverted
overlapLine BottomRight EndTopLeft = TLeft
overlapLine EndBottomLeft EndHLine = TShape
overlapLine BottomLeft EndHLine = TShape
overlapLine TopRight EndHLine = TInverted
overlapLine BottomRight EndBottomLeft = TShape
overlapLine HLine VLine = Cross
overlapLine EndHLine VLine = CrossDot
overlapLine ln ln' = error
	$ "Circut.Diagram.Map.overlapLine: not yet implemented: overlapLine " ++
		show ln ++ " " ++ show ln'

data Dir = T | B | L | R deriving Show

dir :: Pos -> Pos -> Either String Dir
dir p1@(Pos x y) p2@(Pos x' y')
	| x == x', y - 1 == y' = Right T
	| x == x', y + 1 == y' = Right B
	| x - 1 == x', y == y' = Right R
	| x + 1 == x', y == y' = Right L
	| otherwise = Left $ "dir " ++ show p1 ++ " " ++ show p2

dirToLine, dirToLine' :: Dir -> Dir -> Either String ElementDiagram
dirToLine T T = Right VLine
dirToLine T L = Right BottomLeft
dirToLine T R = Right BottomRight
dirToLine B B = Right VLine
dirToLine B L = Right TopLeft
dirToLine B R = Right TopRight
dirToLine L T = Right TopRight
dirToLine L B = Right BottomRight
dirToLine L L = Right HLine
dirToLine R T = Right TopLeft
dirToLine R B = Right BottomLeft
dirToLine R R = Right HLine
dirToLine d d' = Left $ "dirToLine " ++ show d ++ " " ++ show d'

dirToLine' T L = Right EndBottomLeft
dirToLine' B L = Right EndTopLeft
dirToLine' L L = Right EndHLine
dirToLine' R L = Right EndHLineR
dirToLine' d d' = Left $ "dirToLine' " ++ show d ++ " " ++ show d'
