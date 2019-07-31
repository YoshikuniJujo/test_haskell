{-# LANGUAGE TupleSections, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Map where

import Prelude as P

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.Map.Strict
import Data.Bool

import AStar.AStar

data DiagramMap = DiagramMap {
	width :: Int,
	height :: Int,
	layout :: Map Pos Element }
	deriving Show

data Pos = Pos { posX :: Int, posY :: Int } deriving (Show, Eq, Ord)

mkDiagramMap :: Int -> Int -> DiagramMap
mkDiagramMap w h = DiagramMap { width = w, height = h, layout = empty }

data Element
	= Stump
	| AndGateE | OrGateE | NotGateE
	| HLine | VLine
	| TopLeft | TopRight | BottomLeft | BottomRight
	| TShape | TInverted | TLeft | TRight | CrossDot | Cross
	deriving Show

newtype ElementId = ElementId Word deriving (Show, Eq, Ord)

data DiagramMapState = DiagramMapState {
	space :: Int,
	place :: Map Int Int,
	elementPos :: Map ElementId LinePos,
	diagramMap :: DiagramMap }
	deriving Show

initDiagramMapState :: Int -> Int -> DiagramMapState
initDiagramMapState w h = DiagramMapState {
	space = 2,
	place = empty,
	elementPos = empty,
	diagramMap = mkDiagramMap w h }

type DiagramMapM = StateT DiagramMapState Maybe

runDiagramMapM :: Int -> Int -> DiagramMapM a -> Maybe (a, DiagramMap)
runDiagramMapM w h dmm =
	second diagramMap <$> dmm `runStateT` initDiagramMapState w h

generateDiagramMap :: Int -> Int -> DiagramMapM a -> Maybe DiagramMap
generateDiagramMap w h dmm =
	diagramMap <$> dmm `execStateT` initDiagramMapState w h

putElement0, putElement :: ElementId -> Element -> Int -> DiagramMapM Bool
putElement0 = putElementGen True
putElement = putElementGen False

putElementGen :: Bool -> ElementId -> Element -> Int -> DiagramMapM Bool
putElementGen b eid e x = do
	stt <- get
	let	sp = space stt
		y = fromMaybe 0 $ place stt !? x
		p = Pos x y
		dm = diagramMap stt
		l = layout dm
		l' = stump e p $ insert p e l
		l'' = bool l' (insert (Pos (x - 1) y) HLine l') b
		(w, h) = elementSpace e
	lp <- lift $ linePos e p
	put stt {
		place = P.foldr (`insert` (y + h + fromIntegral sp))
			(place stt) [x, x + w - 1],
		elementPos = insert eid lp $ elementPos stt,
		diagramMap = dm { layout = l'' } }
	return True

getElementPos :: ElementId -> DiagramMapM LinePos
getElementPos eid = lift =<< gets ((!? eid) . elementPos)

stump :: Element -> Pos -> Map Pos Element -> Map Pos Element
stump e p m = P.foldr (flip insert Stump) m
	[ Pos x y |
		x <- [x0 .. x0 + w - 1],
		y <- [y0 - h' .. y0 + h'],
		(x, y) /= (x0, y0) ]
	where
	(w, h) = elementSpace e
	h' = (h - 1) `div` 2
	(x0, y0) = (posX p, posY p)

elementSpace :: Element -> (Int, Int)
elementSpace AndGateE = (3, 3)
elementSpace OrGateE = (3, 3)
elementSpace NotGateE = (2, 3)
elementSpace _ = (1, 1)

data LinePos = LinePos { outputLinePos :: Pos, inputLinePos :: [Pos] }
	deriving Show

linePos :: Element -> Pos -> Maybe LinePos
linePos AndGateE (Pos x y) = Just LinePos {
	outputLinePos = Pos (x - 1) y,
	inputLinePos = [Pos (x + 3) (y - 1), Pos (x + 3) (y + 1)] }
linePos OrGateE p = linePos AndGateE p
linePos NotGateE (Pos x y) =
	Just LinePos { outputLinePos = Pos (x - 1) y, inputLinePos = [Pos (x + 2) y] }
linePos _ _ = Nothing

data DiagramMapAStar = DiagramMapAStar {
	startLine :: Pos, endLine :: Pos, diagramMapA :: DiagramMap }
	deriving Show

distance :: Pos -> Pos -> Dist
distance (Pos x y) (Pos x' y') = fromIntegral $ abs (x - x') + abs (y - y')

instance AStar DiagramMapAStar where
	type AStarNode DiagramMapAStar = Pos
	startNode = startLine
	isEndNode = (==) . endLine
	nextNodes = (((, 1) <$>) .) . nextPosDiagramMap
	distToEnd = distance . endLine

nextPosDiagramMap :: DiagramMapAStar -> Pos -> [Pos]
nextPosDiagramMap dma (Pos x0 y0) = [ p |
	p@(Pos x y) <- [Pos (x0 - 1) y0, Pos (x0 + 1) y0, Pos x0 (y0 - 1), Pos x0 (y0 + 1)],
	0 <= x, x < w, 0 <= y, y < h,
	y == y0 && checkHorizontal l p || x == x0 && checkVertical l p ]
	where
	dm = diagramMapA dma
	l = layout dm
	w = width dm
	h = height dm

checkHorizontal, checkVertical :: Map Pos Element -> Pos -> Bool
checkHorizontal l p = case l !? p of
	Just VLine -> True
	Just _ -> False
	Nothing -> True

checkVertical l p = case  l !? p of
	Just HLine -> True
	Just _ -> False
	Nothing -> True

data Dir = T | B | L | R deriving Show

dir :: Pos -> Pos -> Maybe Dir
dir (Pos x y) (Pos x' y')
	| x == x', y - 1 == y' = Just T
	| x == x', y + 1 == y' = Just B
	| x - 1 == x', y == y' = Just R
	| x + 1 == x', y == y' = Just L
	| otherwise = Nothing

dirToLine :: Dir -> Dir -> Maybe Element
dirToLine T T = Just VLine
dirToLine T L = Just BottomLeft
dirToLine T R = Just BottomRight
dirToLine B B = Just VLine
dirToLine B L = Just TopLeft
dirToLine B R = Just TopRight
dirToLine L T = Just TopRight
dirToLine L B = Just BottomRight
dirToLine L L = Just HLine
dirToLine R T = Just TopLeft
dirToLine R B = Just BottomLeft
dirToLine R R = Just HLine
dirToLine _ _ = Nothing

posToLine :: Dir -> [Pos] -> Maybe [Element]
posToLine _ [] = Just []
posToLine d [_] = (: []) <$> dirToLine d L
posToLine d (x : xs@(y : _)) = do
	d' <- dir x y; (:) <$> dirToLine d d' <*> posToLine d' xs

insertLine :: [Pos] -> Map Pos Element -> Maybe (Map Pos Element)
insertLine ps m = P.foldr (uncurry insert) m . zip ps <$> posToLine L ps

connectLine :: Pos -> Pos -> DiagramMapM ()
connectLine p1 p2 = do
	stt <- get
	let	dm = diagramMap stt
		l = layout dm
	ps <- lift $ astar DiagramMapAStar {
		startLine = p1, endLine = p2, diagramMapA = dm }
	l' <- lift $ insertLine ps l
	put stt { diagramMap = dm { layout = l' } }
