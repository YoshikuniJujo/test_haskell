{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Map where

import Prelude as P

import Control.Monad.State
import Data.Maybe
import Data.Map.Strict

data DiagramMap = DiagramMap {
	width :: Word,
	height :: Word,
	layout :: Map (Word, Word) Element }
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

nextLevel :: DiagramMapM ()
nextLevel = do
	stt <- get
	put stt { placeX = placeX stt + 3 }

putElement :: Element -> DiagramMapM ()
putElement e = do
	stt <- get
	let	sp = space stt
		x = placeX stt
		y = fromMaybe 0 $ place stt !? x
		dm = diagramMap stt
		l = layout dm
		l' = stump e x y $ insert (x, y) e l
		(_w, h) = elementSpace e
	put stt {
		place = insert x (y + h + fromIntegral sp) $ place stt,
		diagramMap = dm { layout = l' } }

stump :: Element -> Word -> Word ->
	Map (Word, Word) Element -> Map (Word, Word) Element
stump e x0 y0 m = P.foldr (flip insert Stump) m
	[ (x, y) |
		x <- [x0 .. x0 + w],
		y <- [y0 .. y0 + h],
		(x, y) /= (x0, y0) ]
	where
	(w, h) = elementSpace e

elementSpace :: Element -> (Word, Word)
elementSpace AndGateE = (3, 3)
elementSpace OrGateE = (3, 3)
elementSpace NotGateE = (2, 3)
elementSpace _ = (1, 1)
