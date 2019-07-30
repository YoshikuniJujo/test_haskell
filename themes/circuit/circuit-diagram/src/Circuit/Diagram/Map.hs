{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Map where

import Prelude as P

import Control.Monad.State
import Data.Maybe
import Data.Map.Strict

data DiagramMap = DiagramMap {
	width :: Word,
	height :: Int,
	layout :: Map (Word, Int) Element }
	deriving Show

mkDiagramMap :: Word -> Int -> DiagramMap
mkDiagramMap w h = DiagramMap { width = w, height = h, layout = empty }

data Element
	= Stump
	| AndGateE | OrGateE | NotGateE
	| HLine | VLine
	| TopLeft | TopRight | BottomLeft | BottomRight
	| TShape | TInverted | TLeft | TRight | CrossDot | Cross
	deriving Show

data DiagramMapState = DiagramMapState {
	placeX :: Word,
	space :: Map Word Int,
	diagramMap :: DiagramMap }
	deriving Show

initDiagramMapState :: Word -> Int -> DiagramMapState
initDiagramMapState w h = DiagramMapState {
	placeX = 0,
	space = empty,
	diagramMap = mkDiagramMap w h }

type DiagramMapM = StateT DiagramMapState Maybe

generateDiagramMap :: Word -> Int -> DiagramMapM a -> Maybe DiagramMap
generateDiagramMap w h dmm =
	diagramMap <$> dmm `execStateT` initDiagramMapState w h

nextLevel :: DiagramMapM ()
nextLevel = do
	stt <- get
	put stt { placeX = placeX stt + 3 }

putElement :: Word -> Element -> DiagramMapM ()
putElement sp e = do
	stt <- get
	let	x = placeX stt
		y = fromMaybe 0 $ space stt !? x
		dm = diagramMap stt
		l = layout dm
		l' = stump e x y $ insert (x, y) e l
		(_w, h) = elementSpace e
	put stt {
		space = insert x (y + h + fromIntegral sp) $ space stt,
		diagramMap = dm { layout = l' } }

stump :: Element -> Word -> Int ->
	Map (Word, Int) Element -> Map (Word, Int) Element
stump e x0 y0 m = P.foldr (flip insert Stump) m
	[ (x, y) |
		x <- [x0 .. x0 + w],
		y <- [y0 - h' .. y0 + h'],
		(x, y) /= (x0, y0) ]
	where
	(w, h) = elementSpace e
	h' = (h - 1) `div` 2

elementSpace :: Element -> (Word, Int)
elementSpace AndGateE = (3, 3)
elementSpace OrGateE = (3, 3)
elementSpace NotGateE = (2, 3)
elementSpace _ = (1, 1)
