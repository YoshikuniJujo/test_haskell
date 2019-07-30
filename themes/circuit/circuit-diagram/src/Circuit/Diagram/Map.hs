{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Map where

import Prelude as P

import Control.Monad.State
import Data.Maybe
import Data.Map.Strict

data DiagramMap = DiagramMap {
	width :: Word,
	height :: Word,
	layout :: Map Pos Element }
	deriving Show

mkDiagramMap :: Word -> Word -> DiagramMap
mkDiagramMap w h = DiagramMap { width = w, height = h, layout = empty }

data Element
	= Stump
	| AndGateE | OrGateE | NotGateE
	| HLine | VLine
	| TopLeft | TopRight | BottomLeft | BottomRight
	| TShape | TInverted | TLeft | TRight | CrossDot | Cross
	deriving Show

data DiagramMapState = DiagramMapState {
	space :: Word,
	placeX :: Word,
	place :: Map Word Word,
	diagramMap :: DiagramMap }
	deriving Show

initDiagramMapState :: Word -> Word -> DiagramMapState
initDiagramMapState w h = DiagramMapState {
	space = 1,
	placeX = 0,
	place = empty,
	diagramMap = mkDiagramMap w h }

type DiagramMapM = StateT DiagramMapState Maybe

generateDiagramMap :: Word -> Word -> DiagramMapM a -> Maybe DiagramMap
generateDiagramMap w h dmm =
	diagramMap <$> dmm `execStateT` initDiagramMapState w h

data Pos = Pos { posX :: Word, posY :: Word } deriving (Show, Eq, Ord)

nextLevel :: DiagramMapM ()
nextLevel = do
	stt <- get
	put stt { placeX = placeX stt + 3 }

putElement :: Element -> DiagramMapM LinePos
putElement e = do
	stt <- get
	let	sp = space stt
		x = placeX stt
		y = fromMaybe 0 $ place stt !? x
		p = Pos x y
		dm = diagramMap stt
		l = layout dm
		l' = stump e p $ insert p e l
		(_w, h) = elementSpace e
	put stt {
		place = insert x (y + h + fromIntegral sp) $ place stt,
		diagramMap = dm { layout = l' } }
	lift $ linePos e p

stump :: Element -> Pos -> Map Pos Element -> Map Pos Element
stump e p m = P.foldr (flip insert Stump) m
	[ Pos x y |
		x <- [x0 .. x0 + w],
		y <- [y0 .. y0 + h],
		(x, y) /= (x0, y0) ]
	where
	(w, h) = elementSpace e
	(x0, y0) = (posX p, posY p)

elementSpace :: Element -> (Word, Word)
elementSpace AndGateE = (3, 3)
elementSpace OrGateE = (3, 3)
elementSpace NotGateE = (2, 3)
elementSpace _ = (1, 1)

data LinePos = LinePos { outputLinePos :: Pos, inputLinePos :: [Pos] }
	deriving Show

linePos :: Element -> Pos -> Maybe LinePos
linePos AndGateE p@(Pos x y) = Just LinePos {
	outputLinePos = p,
	inputLinePos = [Pos (x + 2) (y - 1), Pos (x + 2) (y + 1)] }
linePos OrGateE p = linePos AndGateE p
linePos NotGateE p@(Pos x y) =
	Just LinePos { outputLinePos = p, inputLinePos = [Pos (x + 1) y] }
linePos _ _ = Nothing
