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
	| Branch
	| HLine | VLine
	| TopLeft | TopRight | BottomLeft | BottomRight
	| EndHLine | EndHLineR
	| EndBottomLeft
	| TShape | TInverted | TLeft | TRight | CrossDot | Cross
	| HLineText String String
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

getDiagramMap :: DiagramMapM DiagramMap
getDiagramMap = gets diagramMap

getElementFromPos :: Pos -> DiagramMapM (Maybe Element)
getElementFromPos pos = do
	dm <- getDiagramMap
	return $ layout dm !? pos

runDiagramMapM :: Int -> Int -> DiagramMapM a -> Maybe (a, DiagramMap)
runDiagramMapM w h dmm =
	second diagramMap <$> dmm `runStateT` initDiagramMapState w h

generateDiagramMap :: Int -> Int -> DiagramMapM a -> Maybe DiagramMap
generateDiagramMap w h dmm =
	diagramMap <$> dmm `execStateT` initDiagramMapState w h

putElement0, putElement :: ElementId -> Element -> Int -> DiagramMapM Bool
putElement0 eid e x = putElementGen True eid e x Nothing
putElement eid e x = putElementGen False eid e x Nothing

putElementWithPos :: ElementId -> Element -> Pos -> DiagramMapM Bool
putElementWithPos eid e (Pos x y) = putElementGen False eid e x (Just y)

putElementGen :: Bool -> ElementId -> Element -> Int -> Maybe Int -> DiagramMapM Bool
putElementGen b eid e x my_ = do
	me <- gets ((!? eid) . elementPos)
	(\pe -> maybe pe (const $ return False) me) $ do
		my <- do
			case my_ of
				Just y -> bool Nothing my_ <$> placeable e (Pos x y)
				Nothing -> return my_
		stt <- get
		let	sp = space stt
			y = fromMaybe 1 $ place stt !? x
			p = Pos x $ fromMaybe y my
	
			dm = diagramMap stt
			l = layout dm
			l' = stump e p $ insert p e l
			l'' = bool l' (insert (Pos (x - 1) $ posY p) HLine l') b
			(w, h) = elementSpace e
		lp <- lift $ linePos e p
		put stt {
			place = P.foldr (`insert` (max y (posY p) + h + fromIntegral sp))
				(place stt) [x .. x + w - 1],
			elementPos = insert eid lp $ elementPos stt,
			diagramMap = dm { layout = l'' } }
		return True

getElementPos :: ElementId -> DiagramMapM LinePos
getElementPos eid = lift =<< gets ((!? eid) . elementPos)

addElementOutputPos :: ElementId -> [Pos] -> DiagramMapM ()
addElementOutputPos eid ps = do
	st <- get
	let	eps = elementPos st
	lps <- lift $ eps !? eid
	let	lps' = lps { outputLinePos = outputLinePos lps ++ ps }
		eps' = insert eid lps' eps
	put $ st { elementPos = eps' }


stump :: Element -> Pos -> Map Pos Element -> Map Pos Element
stump e p m = P.foldr (flip insert Stump) m
	[ Pos x y |
		x <- [x0 .. x0 + w - 1],
		y <- [y0 - h' .. y0 + h''],
		(x, y) /= (x0, y0) ]
	where
	(w, h) = elementSpace e
	h' = (h - 1) `div` 2
	h'' = h `div` 2
	(x0, y0) = (posX p, posY p)

elementSpace :: Element -> (Int, Int)
elementSpace AndGateE = (3, 3)
elementSpace OrGateE = (3, 3)
elementSpace NotGateE = (2, 3)
elementSpace Branch = (1, 2)
elementSpace _ = (1, 1)

elementToPositions :: Element -> Pos -> [Pos]
elementToPositions e (Pos x0 y0) = [ Pos x y |
	x <- [x0 .. x0 + w - 1],
	y <- [y0 - h' .. y0 + h'] ]
	where
	(w, h) = elementSpace e
	h' = (h - 1) `div` 2

placeable :: Element -> Pos -> DiagramMapM Bool
placeable e pos = and <$> placeablePos `mapM` elementToPositions e pos

placeablePos :: Pos -> DiagramMapM Bool
placeablePos pos = isNothing <$> getElementFromPos pos

data LinePos = LinePos { outputLinePos :: [Pos], inputLinePos :: [Pos] }
	deriving Show

linePos :: Element -> Pos -> Maybe LinePos
linePos AndGateE (Pos x y) = Just LinePos {
	outputLinePos = [Pos (x - 1) y],
	inputLinePos = [Pos (x + 3) (y - 1), Pos (x + 3) (y + 1)] }
linePos OrGateE p = linePos AndGateE p
linePos NotGateE (Pos x y) =
	Just LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 2) y] }
linePos (HLineText _ _) (Pos x y) =
	Just LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 1) y] }
linePos Branch (Pos x y) =
	Just LinePos {
		outputLinePos = [Pos (x - 1) y],
		inputLinePos = [Pos (x + 1) y, Pos (x + 1) (y + 1)] }
linePos _ _ = Nothing

data DiagramMapAStar = DiagramMapAStar {
	startLine :: Pos, endLine :: [Pos], diagramMapA :: DiagramMap }
	deriving Show

distance :: [Pos] -> Pos -> Dist
distance ps p = minimum $ distance1 p <$> ps

distance1 :: Pos -> Pos -> Dist
distance1 (Pos x y) (Pos x' y') = fromIntegral $ abs (x - x') + abs (y - y')

instance AStar DiagramMapAStar where
	type AStarNode DiagramMapAStar = Pos
	startNode = startLine
	isEndNode = flip elem . endLine
	nextNodes = (((, 1) <$>) .) . nextPosDiagramMap
	distToEnd = distance . endLine

nextPosDiagramMap :: DiagramMapAStar -> Pos -> [Pos]
nextPosDiagramMap dma (Pos x0 y0) = [ p |
	p@(Pos x y) <- [Pos (x0 - 1) y0, Pos (x0 + 1) y0, Pos x0 (y0 - 1), Pos x0 (y0 + 1)],
	0 <= x, x < w, 0 <= y, y < h,
	isEndNode dma (Pos x y) || y == y0 && checkHorizontal l p || x == x0 && checkVertical l p ]
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
	Just EndHLine -> True
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

dirToLine, dirToLine' :: Dir -> Dir -> Maybe Element
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
dirToLine d d' = error $ "dirToLine " ++ show d ++ " " ++ show d'
-- dirToLine _ _ = Nothing

dirToLine' T L = Just EndBottomLeft
dirToLine' B L = Just TopLeft
dirToLine' L L = Just EndHLine
dirToLine' R L = Just EndHLineR
dirToLine' d d' = error $ "dirToLine' " ++ show d ++ " " ++ show d'
-- dirToLine' _ _ = Nothing

posToLine :: Dir -> [Pos] -> Maybe [Element]
posToLine _ [] = Just []
posToLine d [_] = (: []) <$> dirToLine' d L
posToLine d (x : xs@(y : _)) = do
	d' <- dir x y; (:) <$> dirToLine d d' <*> posToLine d' xs

insertLine :: [Pos] -> Map Pos Element -> Maybe (Map Pos Element)
insertLine ps m =
	P.foldr (uncurry overlapInsertLine) m . zip ps <$> posToLine L ps

overlapInsertLine :: Pos -> Element -> Map Pos Element -> Map Pos Element
overlapInsertLine pos ln m = case m !? pos of
	Just ln' -> insert pos (overlapLine ln' ln) m
	Nothing	-- | EndHLine <- ln -> error $ "here: " ++ show pos
		| otherwise -> insert pos ln m

overlapLine :: Element -> Element -> Element
overlapLine HLine EndBottomLeft = TShape
overlapLine EndHLine EndBottomLeft = TShape
overlapLine VLine HLine = Cross
overlapLine VLine EndHLine = TLeft
overlapLine VLine EndHLineR = TRight
overlapLine HLine TopLeft = TInverted
overlapLine BottomRight EndHLineR = TShape
overlapLine ln ln' = error
	$ "Circut.Diagram.Map.overlapLine: not yet implemented: overlapLine " ++
		show ln ++ " " ++ show ln'

connectLine :: Pos -> ElementId -> DiagramMapM ()
connectLine p1 eid = do
	p2 <- outputLinePos <$> getElementPos eid
	ps <- connectLine' p1 p2
	addElementOutputPos eid ps

connectLine' :: Pos -> [Pos] -> DiagramMapM [Pos]
connectLine' p1 p2 = do
	stt <- get
	let	dm = diagramMap stt
		l = layout dm
	ps <- lift $ astar DiagramMapAStar {
		startLine = p1, endLine = p2, diagramMapA = dm }
	l' <- lift $ insertLine ps l
	put stt { diagramMap = dm { layout = l' } }
	return ps
