{-# LANGUAGE TupleSections, TypeFamilies, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Map where

import Prelude as P

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.Map.Strict
import Data.Bool
import Data.String
import Crypto.Hash

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteArray as BA

import AStar.AStar

data DiagramMap = DiagramMap {
	width :: Int,
	height :: Int,
	layout :: Map Pos Element }
	deriving Show

getWidthDiagramMap, getHeightDiagramMap :: DiagramMap -> Int
getWidthDiagramMap = width
getHeightDiagramMap = height

setWidthDiagramMap, setHeightDiagramMap :: DiagramMap -> Int -> DiagramMap
setWidthDiagramMap d w = d { width = w }
setHeightDiagramMap d h = d { height = h }

data Pos = Pos { posX :: Int, posY :: Int } deriving (Show, Eq, Ord)

mkDiagramMap :: Int -> Int -> DiagramMap
mkDiagramMap w h = DiagramMap { width = w, height = h, layout = empty }

data Element
	= Stump
	| AndGateE | OrGateE | NotGateE
	| BranchE
	| HLine | VLine
	| TopLeft | TopRight | BottomLeft | BottomRight
	| EndHLine | EndHLineR | EndTopLeft
	| EndBottomLeft
	| TShape | TInverted | TLeft | TRight | CrossDot | Cross
	| HLineText String String
	deriving Show

newtype ElementId = ElementId BS.ByteString deriving (Show, Eq, Ord)

instance IsString ElementId where
	fromString = ElementId . BA.convert . hash @_ @SHA3_256 . BSC.pack

class ElementIdable a where
	elementIdGen :: a -> BS.ByteString

elementId :: ElementIdable eid => eid -> ElementId
elementId = ElementId . elementIdGen

instance ElementIdable ElementId where
	elementIdGen (ElementId bs) = bs

data DiagramMapState = DiagramMapState {
	space :: Int,
	place :: Map Int Int,
	elementPos :: Map ElementId LinePos,
	diagramMap :: DiagramMap }
	deriving Show

initDiagramMapState :: Int -> DiagramMapState
initDiagramMapState sp = DiagramMapState {
	space = sp,
	place = empty,
	elementPos = empty,
	diagramMap = mkDiagramMap 0 0 }

updatePlaceDMState :: DiagramMapState -> Int -> Int -> DiagramMapState
updatePlaceDMState dms x y = dms { place = insert x y' ep }
	where
	ep = place dms
	my0 = ep !? x
	y' = maybe (y + 1) (max $ y + 1) $ my0

getWidthDMState, getHeightDMState :: DiagramMapState -> Int
getWidthDMState = getWidthDiagramMap . diagramMap
getHeightDMState = getHeightDiagramMap . diagramMap

setWidthDMState, setHeightDMState :: DiagramMapState -> Int -> DiagramMapState
setWidthDMState dms w =
	dms { diagramMap = setWidthDiagramMap (diagramMap dms) w }
setHeightDMState dms h =
	dms { diagramMap = setHeightDiagramMap (diagramMap dms) h }

type DiagramMapM = StateT DiagramMapState (Either String)

getSpace :: DiagramMapM Int
getSpace = gets space

getDiagramMap :: DiagramMapM DiagramMap
getDiagramMap = gets diagramMap

getWidth, getHeight :: DiagramMapM Int
getWidth = gets getWidthDMState
getHeight = gets getHeightDMState

setWidth, setHeight :: Int -> DiagramMapM ()
setWidth = modify . flip setWidthDMState
setHeight = modify . flip setHeightDMState

expandWidth, expandHeight :: Int -> DiagramMapM ()
expandWidth w = setWidth . max w =<< getWidth
expandHeight h = setHeight . max h =<< getHeight

updatePlace :: Int -> Int -> DiagramMapM ()
updatePlace = (modify .) . curry (flip $ uncurry . updatePlaceDMState)

inputPosition :: LinePos -> DiagramMapM Pos
inputPosition lp = lift . calcInputPosition lp =<< getSpace

inputPosition1, inputPosition2 :: LinePos -> DiagramMapM Pos
inputPosition1 lp = lift . calcInputPosition1 lp =<< getSpace
inputPosition2 lp = lift . calcInputPosition2 lp =<< getSpace

getElementFromPos :: Pos -> DiagramMapM (Maybe Element)
getElementFromPos pos = do
	dm <- getDiagramMap
	return $ layout dm !? pos

runDiagramMapM :: DiagramMapM a -> Int -> Either String (a, DiagramMap)
runDiagramMapM dmm sp =
	second diagramMap <$> dmm `runStateT` initDiagramMapState sp

execDiagramMapM :: DiagramMapM a -> Int -> Either String DiagramMap
execDiagramMapM dmm sp =
	diagramMap <$> dmm `execStateT` initDiagramMapState sp

newElement0 :: ElementIdable eid => eid -> Element -> DiagramMapM LinePos
newElement0 eid e = maybe (lift $ Left "Oops!") return =<< putElement0 eid e

newElementWithPos ::
	ElementIdable ied => ied -> Element -> Pos -> DiagramMapM LinePos
newElementWithPos eid e p =
	maybe (lift $ Left "Oops!") return =<< putElementWithPos eid e p

putElement0 :: ElementIdable eid => eid -> Element -> DiagramMapM (Maybe LinePos)
putElement0 eid e = putElementGen True eid e 2 Nothing

putElement :: ElementIdable eid => eid -> Element -> Int -> DiagramMapM (Maybe LinePos)
putElement eid e x = putElementGen False eid e x Nothing

putElementWithPos :: ElementIdable eid => eid -> Element -> Pos -> DiagramMapM (Maybe LinePos)
putElementWithPos eid e (Pos x y) = putElementGen False eid e x (Just y)

putElementGen :: ElementIdable eid => Bool -> eid -> Element -> Int -> Maybe Int -> DiagramMapM (Maybe LinePos)
putElementGen b eidg e x my_ = do
	me <- gets ((!? elementId eidg) . elementPos)
	(\pe -> maybe pe (const $ return Nothing) me) $ do
		let	(w, h) = elementSpace e
		my <- do
			case my_ of
				Just y -> bool Nothing my_ . ((y > (w - 1) `div` 2) &&) <$> placeable e (Pos x y)
				Nothing -> return my_
		stt <- get
		let	sp = space stt
			y = fromMaybe 1 $ place stt !? x
			p = Pos x $ fromMaybe y my
	
			dm = diagramMap stt
			l = layout dm
			l' = stump e p $ insert p e l
			l'' = bool l' (insert (Pos (x - 1) $ posY p) HLine l') b
		lp <- lift $ linePos e p
		put stt {
			place = P.foldr (`insert` (max y (posY p) + h + fromIntegral sp))
				(place stt) [x .. x + w - 1],
			elementPos = insert (elementId eidg) lp $ elementPos stt,
			diagramMap = dm { layout = l'' } }
		expandWidth $ posX p + w + sp
		expandHeight $ posY p + h + sp
		return $ Just lp

getElementPos :: ElementIdable eid => eid -> DiagramMapM LinePos
getElementPos eidg = lift
	=<< gets (maybe (Left emsg) Right . (!? elementId eidg) . elementPos)
	where emsg = "No such element: " ++
		"Circuit.Diagram.Map.getElementPos " ++ show (elementId eidg)

getInputPos :: ElementIdable eid => eid -> DiagramMapM [Pos]
getInputPos = (inputLinePos <$>) . getElementPos

addElementOutputPos :: ElementId -> [Pos] -> DiagramMapM ()
addElementOutputPos eid ps = do
	st <- get
	let	eps = elementPos st
	lps <- lift $ maybe
		(Left $ "addElementOutputPos " ++ show eid ++ " " ++ show ps)
		Right $ eps !? eid
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
elementSpace BranchE = (1, 2)
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

calcInputPosition, calcInputPosition1, calcInputPosition2 :: LinePos -> Int -> Either String Pos
calcInputPosition LinePos { inputLinePos = [ip] } dx = Right $ Pos (posX ip + dx) (posY ip)
calcInputPosition lp dx = Left $ "calcInputPosition " ++ show lp ++ " " ++ show dx

calcInputPosition1 LinePos { inputLinePos = [ip, _] } dx = Right $ Pos (posX ip + dx) (posY ip)
calcInputPosition1 lp dx = Left $ "calcInputPosition1 " ++ show lp ++ " " ++ show dx

calcInputPosition2 LinePos { inputLinePos = [_, ip] } dx = Right $ Pos (posX ip + dx) (posY ip)
calcInputPosition2 lp dx = Left $ "calcInputPosition2 " ++ show lp ++ " " ++ show dx

linePos :: Element -> Pos -> Either String LinePos
linePos AndGateE (Pos x y) = Right LinePos {
	outputLinePos = [Pos (x - 1) y],
	inputLinePos = [Pos (x + 3) (y - 1), Pos (x + 3) (y + 1)] }
linePos OrGateE p = linePos AndGateE p
linePos NotGateE (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 2) y] }
linePos (HLineText _ _) (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 1) y] }
linePos BranchE (Pos x y) =
	Right LinePos {
		outputLinePos = [Pos (x - 1) y],
		inputLinePos = [Pos (x + 1) y, Pos (x + 1) (y + 1)] }
linePos e pos = Left $ "linePos " ++ show e ++ " " ++ show pos

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

dir :: Pos -> Pos -> Either String Dir
dir p1@(Pos x y) p2@(Pos x' y')
	| x == x', y - 1 == y' = Right T
	| x == x', y + 1 == y' = Right B
	| x - 1 == x', y == y' = Right R
	| x + 1 == x', y == y' = Right L
	| otherwise = Left $ "dir " ++ show p1 ++ " " ++ show p2

dirToLine, dirToLine' :: Dir -> Dir -> Either String Element
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

posToLine :: Dir -> [Pos] -> Either String [Element]
posToLine _ [] = Right []
posToLine d [_] = (: []) <$> dirToLine' d L
posToLine d (x : xs@(y : _)) = do
	d' <- dir x y; (:) <$> dirToLine d d' <*> posToLine d' xs

insertLine :: [Pos] -> Map Pos Element -> Either String (Map Pos Element)
insertLine ps m =
	P.foldr (uncurry overlapInsertLine) m . zip ps <$> posToLine L ps

overlapInsertLine :: Pos -> Element -> Map Pos Element -> Map Pos Element
overlapInsertLine pos ln m = case m !? pos of
	Just ln' -> insert pos (overlapLine ln' ln) m
	Nothing -> insert pos ln m

overlapLine :: Element -> Element -> Element
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

connectLine :: ElementIdable eid => eid -> Int -> eid -> DiagramMapM ()
connectLine ei i eo = (`connectLine'` eo) . (!! i) =<< getInputPos ei

connectLine' :: ElementIdable eid => Pos -> eid -> DiagramMapM ()
connectLine' p1 eidg = do
	p2 <- outputLinePos <$> getElementPos eidg
	ps <- connectLineGen p1 p2
	addElementOutputPos (elementId eidg) ps

connectLineGen :: Pos -> [Pos] -> DiagramMapM [Pos]
connectLineGen p1 p2 = do
	stt <- get
	let	dm = diagramMap stt
		l = layout dm
	ps <- lift $ maybe (Left $ emsg dm) Right $ astar DiagramMapAStar {
		startLine = p1, endLine = p2, diagramMapA = dm }
	l' <- lift $ insertLine ps l
	put stt { diagramMap = dm { layout = l' } }
	(\(Pos x y) -> updatePlace x y) `mapM_` ps
	return ps
	where emsg = ("astar: no route: " ++) . show
