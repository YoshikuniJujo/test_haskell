{-# LANGUAGE TupleSections, TypeFamilies, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Map (
	DiagramMapM, DiagramMapState, runDiagramMapM, execDiagramMapM,
	DiagramMap, ElementIdable(..), ElementId,
	ElementDiagram,
	andGateD, orGateD, notGateD, triGateD, constGateD, delayD,
	hLineD, hLineTextD, branchD,
	Pos, LinePos,
	putElement0, putElement, newElement0, newElement,
	inputPosition, inputPosition1, inputPosition2,
	connectLine, connectLine1, connectLine2 ) where

import Prelude as P

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.Map.Strict
import Data.Bool
import Data.Word
import Data.String
import Crypto.Hash

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteArray as BA

import AStar.AStar
import Circuit.Diagram.DiagramMap

andGateD, orGateD, notGateD, triGateD, hLineD, branchD :: ElementDiagram
[andGateD, orGateD, notGateD, triGateD, hLineD, branchD] =
	[AndGateE, OrGateE, NotGateE, TriGateE, HLine, BranchE]

hLineTextD :: String -> String -> ElementDiagram
hLineTextD = HLineText

constGateD :: Word64 -> ElementDiagram
constGateD = ConstGateE

delayD :: Word8 -> ElementDiagram
delayD = DelayE

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

getElementFromPos :: Pos -> DiagramMapM (Maybe ElementDiagram)
getElementFromPos pos = do
	dm <- getDiagramMap
	return $ layout dm !? pos

runDiagramMapM :: DiagramMapM a -> Int -> Either String (a, DiagramMap)
runDiagramMapM dmm sp =
	second diagramMap <$> dmm `runStateT` initDiagramMapState sp

execDiagramMapM :: DiagramMapM a -> Int -> Either String DiagramMap
execDiagramMapM dmm sp =
	diagramMap <$> dmm `execStateT` initDiagramMapState sp

newElement0 :: ElementIdable eid => eid -> ElementDiagram -> DiagramMapM LinePos
newElement0 eid e = maybe (lift $ Left "Oops!") return =<< putElement0 eid e

newElement :: ElementIdable ied => ied -> ElementDiagram -> Pos -> DiagramMapM LinePos
newElement eid e p = maybe (lift $ Left "Oops!") return =<< putElement eid e p

putElement0 :: ElementIdable eid => eid -> ElementDiagram -> DiagramMapM (Maybe LinePos)
putElement0 eid e = putElementGen True eid e 2 Nothing

putElement :: ElementIdable eid => eid -> ElementDiagram -> Pos -> DiagramMapM (Maybe LinePos)
putElement eid e (Pos x y) = putElementGen False eid e x (Just y)

putElementGen :: ElementIdable eid => Bool -> eid -> ElementDiagram -> Int -> Maybe Int -> DiagramMapM (Maybe LinePos)
putElementGen b eidg e x my_ = do
	me <- gets ((!? elementId eidg) . elementPos)
	(\pe -> maybe pe (const $ return Nothing) me) $ do
		let	(w, (h, h')) = elementSpace e
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
			place = P.foldr (`insert` (max y (posY p) + h + h' + 1 + fromIntegral sp))
				(place stt) [x .. x + w - 1],
			elementPos = insert (elementId eidg) lp $ elementPos stt,
			diagramMap = dm { layout = l'' } }
		expandWidth $ posX p + w + sp
		expandHeight $ posY p + h + h' + 1 + sp
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

placeable :: ElementDiagram -> Pos -> DiagramMapM Bool
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

linePos :: ElementDiagram -> Pos -> Either String LinePos
linePos AndGateE (Pos x y) = Right LinePos {
	outputLinePos = [Pos (x - 1) y],
	inputLinePos = [Pos (x + 3) (y - 1), Pos (x + 3) (y + 1)] }
linePos OrGateE p = linePos AndGateE p
linePos NotGateE (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 2) y] }
linePos TriGateE (Pos x y) =
	Right LinePos {
		outputLinePos = [Pos (x - 1) y],
		inputLinePos = [Pos (x + 2) (y - 2), Pos (x + 2) y] }
linePos (ConstGateE _) (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [] }
linePos (DelayE _) (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 2) y] }
linePos HLine (Pos x y) =
	Right LinePos { outputLinePos = [Pos (x - 1) y], inputLinePos = [Pos (x + 1) y] }
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

checkHorizontal, checkVertical :: Map Pos ElementDiagram -> Pos -> Bool
checkHorizontal l p = case l !? p of
	Just VLine -> True
	Just _ -> False
	Nothing -> True

checkVertical l p = case  l !? p of
	Just HLine -> True
	Just EndHLine -> True
	Just _ -> False
	Nothing -> True

connectLine, connectLine1, connectLine2 :: ElementIdable eid => eid -> eid -> DiagramMapM ()
connectLine ei eo = (`connectLine'` eo) =<< lift . single =<< getInputPos ei
connectLine1 ei eo = (`connectLine'` eo) =<< lift . oneOfTwo =<< getInputPos ei
connectLine2 ei eo = (`connectLine'` eo) =<< lift . twoOfTwo =<< getInputPos ei

single :: [a] -> Either String a
single [x] = Right x
single _ = Left $ "Circuit.Diagram.Map.single: not single element list"

oneOfTwo, twoOfTwo :: [a] -> Either String a
oneOfTwo [x, _] = Right x
oneOfTwo _ = Left
	$ "Circuit.Diagram.Map.oneOfTwo xs: xs should be include just two elems"

twoOfTwo [_, y] = Right y
twoOfTwo _ = Left
	$ "Circuit.Diagram.Map.twoOfTwo xs: xs should be include just two elems"

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
